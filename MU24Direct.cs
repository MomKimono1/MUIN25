// MU CADPDF V24 - Direct native multi-page PDF via AutoCAD .NET PlotEngine.
// Target AutoCAD 2021, .NET Framework 4.8. No PDF24, DSD, WBLOCK or duplicate DWG.
using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;
using System.Globalization;
using Autodesk.AutoCAD.Runtime;
using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.PlottingServices;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.EditorInput;
using AcApp = Autodesk.AutoCAD.ApplicationServices.Application;

[assembly: CommandClass(typeof(MUCADPDF24.DirectPlot))]

namespace MUCADPDF24
{
    internal sealed class Box
    {
        internal double X1, Y1, X2, Y2;
    }

    internal sealed class Job
    {
        internal string Drawing, Layout, Output, Device, Media, Style;
        internal int Orientation, Scale;
        internal List<Box> Boxes = new List<Box>();
    }

    public class DirectPlot
    {
        private static string JobPath { get { return Path.Combine(Path.GetTempPath(), "MU24_ACTIVE_JOB.txt"); } }
        private static double ReadNum(string s) { return double.Parse(s, CultureInfo.InvariantCulture); }

        private static Job LoadJob(string path)
        {
            string[] l = File.ReadAllLines(path, System.Text.Encoding.UTF8);
            if (l.Length < 10 || l[0].Trim() != "MU24-1")
                throw new InvalidDataException("Tap lenh in V24 khong hop le.");
            Job j = new Job();
            j.Drawing = l[1]; j.Layout = l[2]; j.Output = l[3];
            j.Device = l[4]; j.Media = l[5]; j.Style = l[6];
            j.Orientation = int.Parse(l[7]); j.Scale = int.Parse(l[8]);
            int n = int.Parse(l[9]);
            if (n < 1 || n > 300 || l.Length != 10 + n)
                throw new InvalidDataException("So khung khong khop du lieu.");
            for (int i = 0; i < n; i++)
            {
                string[] p = l[i + 10].Split('\t');
                if (p.Length != 4) throw new InvalidDataException("Loi toa do khung " + (i + 1));
                Box b = new Box();
                b.X1 = ReadNum(p[0]); b.Y1 = ReadNum(p[1]);
                b.X2 = ReadNum(p[2]); b.Y2 = ReadNum(p[3]);
                if (!(b.X2 > b.X1 && b.Y2 > b.Y1))
                    throw new InvalidDataException("Khung " + (i + 1) + " co toa do sai.");
                j.Boxes.Add(b);
            }
            return j;
        }

        // WCS selection points from LISP -> DCS plot-window coordinates.
        private static Matrix3d WcsToDcs(Editor ed)
        {
            using (ViewTableRecord v = ed.GetCurrentView())
            {
                Matrix3d m = Matrix3d.PlaneToWorld(v.ViewDirection);
                m = Matrix3d.Displacement(v.Target - Point3d.Origin) * m;
                m = Matrix3d.Rotation(-v.ViewTwist, v.ViewDirection, v.Target) * m;
                return m.Inverse();
            }
        }

        private static Extents2d PlotBox(Box b, Matrix3d worldToDcs)
        {
            Point3d[] pts = {
                new Point3d(b.X1,b.Y1,0), new Point3d(b.X1,b.Y2,0),
                new Point3d(b.X2,b.Y1,0), new Point3d(b.X2,b.Y2,0)
            };
            double x0=Double.MaxValue,y0=Double.MaxValue,x1=Double.MinValue,y1=Double.MinValue;
            foreach (Point3d p in pts)
            {
                Point3d q = p.TransformBy(worldToDcs);
                x0=Math.Min(x0,q.X); x1=Math.Max(x1,q.X);
                y0=Math.Min(y0,q.Y); y1=Math.Max(y1,q.Y);
            }
            return new Extents2d(x0, y0, x1, y1);
        }

        private static PlotInfo MakePlotInfo(Layout layout, Job job, Box b, Matrix3d worldToDcs)
        {
            PlotSettingsValidator v = PlotSettingsValidator.Current;
            PlotSettings ps = new PlotSettings(layout.ModelType);
            ps.CopyFrom(layout);
            v.SetPlotConfigurationName(ps, job.Device, job.Media);
            v.SetPlotPaperUnits(ps, PlotPaperUnit.Millimeters);
            // AutoCAD requires window coordinates in DCS, not WCS.
            v.SetPlotWindowArea(ps, PlotBox(b, worldToDcs));
            v.SetPlotType(ps, Autodesk.AutoCAD.DatabaseServices.PlotType.Window);
            v.SetPlotCentered(ps, true);
            if (job.Scale == 0)
            {
                v.SetUseStandardScale(ps, true);
                v.SetStdScaleType(ps, StdScaleType.ScaleToFit);
            }
            else
            {
                v.SetUseStandardScale(ps, false);
                v.SetCustomPrintScale(ps, new CustomScale(1, 1));
            }
            if (!String.IsNullOrWhiteSpace(job.Style) && job.Style != "(Khong ap dung)")
            {
                v.SetCurrentStyleSheet(ps, job.Style);
                ps.PlotPlotStyles = true;
            }
            // The media name can be landscape or portrait. Rotate only when necessary.
            string media = job.Media.ToUpperInvariant();
            bool isPortrait = media.IndexOf("297.00_X_420.00", StringComparison.Ordinal) >= 0 ||
                              media.IndexOf("210.00_X_297.00", StringComparison.Ordinal) >= 0;
            bool isLandscape = media.IndexOf("420.00_X_297.00", StringComparison.Ordinal) >= 0 ||
                               media.IndexOf("297.00_X_210.00", StringComparison.Ordinal) >= 0;
            PlotRotation r = PlotRotation.Degrees000;
            if (job.Orientation == 0 && isPortrait) r = PlotRotation.Degrees090;
            if (job.Orientation == 1 && isLandscape) r = PlotRotation.Degrees090;
            v.SetPlotRotation(ps, r);

            PlotInfo pi = new PlotInfo();
            pi.Layout = layout.ObjectId;
            pi.OverrideSettings = ps;
            PlotInfoValidator piv = new PlotInfoValidator();
            piv.MediaMatchingPolicy = MatchingPolicy.MatchEnabled;
            piv.Validate(pi);
            return pi;
        }

        [CommandMethod("MU24CORE", CommandFlags.Modal)]
        public void DirectPdf()
        {
            Document doc = AcApp.DocumentManager.MdiActiveDocument;
            Editor ed = doc.Editor;
            string source = JobPath;
            string oldBackground = Convert.ToString(AcApp.GetSystemVariable("BACKGROUNDPLOT"));
            List<PlotInfo> plotInfos = new List<PlotInfo>();
            string output = null;
            try
            {
                if (!File.Exists(source))
                    throw new FileNotFoundException("Chua co danh sach to do LISP MUIN24 tao.");
                Job job = LoadJob(source);
                if (!String.Equals(Path.GetFullPath(doc.Name), Path.GetFullPath(job.Drawing), StringComparison.OrdinalIgnoreCase))
                    throw new InvalidOperationException("Danh sach in khong thuoc DWG dang mo.");
                if (!String.Equals(LayoutManager.Current.CurrentLayout, job.Layout, StringComparison.OrdinalIgnoreCase))
                    throw new InvalidOperationException("Hay chuyen ve tab " + job.Layout + " truoc khi in.");
                if (!job.Device.EndsWith(".pc3", StringComparison.OrdinalIgnoreCase) ||
                    job.Device.IndexOf("PDF", StringComparison.OrdinalIgnoreCase) < 0)
                    throw new InvalidOperationException("Hay chon AutoCAD PDF.pc3 / DWG To PDF.pc3.");
                if (PlotFactory.ProcessPlotState != ProcessPlotState.NotPlotting)
                    throw new InvalidOperationException("AutoCAD dang xu ly mot tac vu in khac.");
                output = job.Output;
                if (File.Exists(output)) throw new IOException("File dau ra da ton tai, hay doi ten: " + output);
                if (!Directory.Exists(Path.GetDirectoryName(output)))
                    throw new DirectoryNotFoundException("Khong tim thay thu muc luu PDF.");
                AcApp.SetSystemVariable("BACKGROUNDPLOT", 0);
                Matrix3d wcsToDcs = WcsToDcs(ed);
                using (Transaction tr = doc.Database.TransactionManager.StartTransaction())
                {
                    ObjectId layoutId = LayoutManager.Current.GetLayoutId(job.Layout);
                    Layout layout = (Layout)tr.GetObject(layoutId, OpenMode.ForRead);
                    foreach (Box b in job.Boxes)
                        plotInfos.Add(MakePlotInfo(layout, job, b, wcsToDcs));
                    tr.Commit();
                }
                ed.WriteMessage("\n[MU24] Xuat " + plotInfos.Count + " trang vao MOT PDF...");
                using (PlotProgressDialog progress = new PlotProgressDialog(false, plotInfos.Count, true))
                using (PlotEngine engine = PlotFactory.CreatePublishEngine())
                {
                    progress.OnBeginPlot();
                    progress.IsVisible = true;
                    progress.LowerPlotProgressRange = 0;
                    progress.UpperPlotProgressRange = 100;
                    engine.BeginPlot(progress, null);
                    engine.BeginDocument(plotInfos[0], doc.Name, null, 1, true, output);
                    for (int i = 0; i < plotInfos.Count; i++)
                    {
                        progress.OnBeginSheet();
                        progress.LowerSheetProgressRange = 0;
                        progress.UpperSheetProgressRange = 100;
                        progress.SheetProgressPos = 0;
                        using (PlotPageInfo page = new PlotPageInfo())
                        {
                            engine.BeginPage(page, plotInfos[i], i == plotInfos.Count - 1, null);
                            engine.BeginGenerateGraphics(null);
                            engine.EndGenerateGraphics(null);
                            engine.EndPage(null);
                        }
                        progress.SheetProgressPos = 100;
                        progress.OnEndSheet();
                        progress.PlotProgressPos = (int)((i + 1) * 100.0 / plotInfos.Count);
                        ed.WriteMessage("\n[MU24] Trang " + (i + 1) + "/" + plotInfos.Count);
                    }
                    engine.EndDocument(null);
                    progress.PlotProgressPos = 100;
                    engine.EndPlot(null);
                    progress.OnEndPlot();
                }
                if (!File.Exists(output) || new FileInfo(output).Length < 1000)
                    throw new IOException("PlotEngine ket thuc nhung khong tim thay PDF hop le.");
                ed.WriteMessage("\n[MU24] THANH CONG: " + output + "\n");
                AcApp.ShowAlertDialog("DA XUAT 1 PDF NHIEU TRANG\n" + output + "\nKiem tra thu tu trong file PDF.");
            }
            catch (System.Exception ex)
            {
                ed.WriteMessage("\n[MU24 LOI] " + ex.ToString() + "\n");
                AcApp.ShowAlertDialog("MU CADPDF V24: KHONG XUAT DUOC.\n" + ex.Message + "\nNhan F2 xem loi chi tiet.");
            }
            finally
            {
                foreach (PlotInfo pi in plotInfos)
                {
                    try { if (pi.OverrideSettings != null) pi.OverrideSettings.Dispose(); } catch { }
                    try { pi.Dispose(); } catch { }
                }
                try { AcApp.SetSystemVariable("BACKGROUNDPLOT", Int32.Parse(oldBackground)); } catch { }
                try { if (File.Exists(source)) File.Delete(source); } catch { }
            }
        }
    }
}
