with Raylib;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;

procedure Raylib_Gym is

   subtype CFloat is Interfaces.C.C_float;

   function Lerp (start, stop, alpha : CFloat) return CFloat is
      (start + (stop - start) * alpha);

   screenWidth  : constant := 800;
   screenHeight : constant := 450;
   Cam : aliased Raylib.Camera3D;
   Model : Raylib.Model;
   Dt : CFloat;

   Car_Rot_Angle : Cfloat := 0.0;
begin
   Raylib.InitWindow (screenWidth, screenHeight, New_String ("Raylib Gym"));

   Model := Raylib.LoadModel (New_String ("/home/henley/Desktop/GAP_org/raylib_gym/data/race_car.gltf"));

   Cam.position := (10.0, 10.0, 10.0);
   Cam.target := (0.0, 0.0, 0.0);
   Cam.up := (0.0, 1.0, 0.0);
   Cam.fovy := 45.0;
   Cam.projection := Interfaces.C.int (Raylib.CAMERA_PERSPECTIVE);

   Raylib.DisableCursor;
   Raylib.SetTargetFPS (60);

   while not Raylib.WindowShouldClose loop
      Raylib.BeginDrawing;

      Raylib.ClearBackground (Raylib.RAYWHITE);

      Raylib.BeginMode3D (Cam);

      declare
         Pos : Raylib.Vector3 := (0.0, 0.0, 0.0);
         Rot_Axis : Raylib.Vector3 := (0.0, 1.0, 0.0);
         Scale : Raylib.Vector3 := (0.5, 0.5, 0.5);
      begin
         Dt := Raylib.GetFrameTime;
         Car_Rot_Angle := Lerp (Car_Rot_Angle, 360.0, Dt);
         Raylib.DrawModelEx(Model, Pos, Rot_Axis, Car_Rot_Angle, Scale, Raylib.WHITE);
      end;

      Raylib.DrawGrid (10, 1.0);

      Raylib.EndMode3D;

      Raylib.EndDrawing;
   end loop;

   Raylib.CloseWindow;
end Raylib_Gym;