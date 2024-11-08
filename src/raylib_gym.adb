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

   Car : Raylib.Model;
   Race_Track : Raylib.Model;

   Outer_Boundary : Raylib.Model;
   Outer_Boundary_Mesh : Raylib.Mesh;

   Inner_Boundary : Raylib.Model;
   Inner_Boundary_Mesh : Raylib.Mesh;

   World_Pos : Raylib.Vector3 := (0.0, 0.0, 0.0);
   World_Rot_Axis : Raylib.Vector3 := (0.0, 1.0, 0.0);
   World_Scale : Raylib.Vector3 := (0.2, 0.2, 0.2);

   Dt : CFloat;

   Car_Rot_Angle : Cfloat := 0.0;

   Center_BB_Size : Raylib.Vector3 := (1.0, 1.0, 1.0);
   Center_BB_Pos : Raylib.Vector3 := (-Center_BB_Size.x / 2.0, 
                                      0.0, 
                                      -Center_BB_Size.z / 2.0);

   Center_BB : Raylib.BoundingBox := ((0.0, 0.0, 0.0), (100.0, 100.0, 100.0));

begin
   Raylib.InitWindow (screenWidth, screenHeight, New_String ("Raylib Gym"));
   
   Car := Raylib.LoadModel (New_String ("./data/race_car.gltf"));
   Race_Track := Raylib.LoadModel (New_String ("./data/race_track.gltf"));

   Outer_Boundary := Raylib.LoadModel (New_String ("./data/outer_boundary.gltf"));
   Outer_Boundary_Mesh := Outer_Boundary.meshes.all;

   Inner_Boundary := Raylib.LoadModel (New_String ("./data/inner_boundary.gltf"));
   -- RUN THE EXECUTABLE FROM THE ROOT OF THE PROJECT -> bin/raylib_gym

   Cam.position := (10.0, 10.0, 10.0);
   Cam.target := (0.0, 0.0, 0.0);
   Cam.up := (0.0, 1.0, 0.0);
   Cam.fovy := 45.0;
   Cam.projection := Interfaces.C.int (Raylib.CAMERA_PERSPECTIVE);

   Raylib.DisableCursor;
   Raylib.SetTargetFPS (60);

   while not Raylib.WindowShouldClose loop
      Raylib.BeginDrawing;

      -- WORKING in 2D. So 0,0 is the top left corner. (ScreenSpace)

      Raylib.ClearBackground (Raylib.RAYWHITE);

      Raylib.BeginMode3D (Cam);

      -- WORKING in 3D. So 0,0 is the center of the grid. (WorldSpace)

      --Raylib.DrawModelEx (Race_Track,     World_Pos, World_Rot_Axis, 0.0, World_Scale, Raylib.WHITE);
      Raylib.DrawModelEx (Outer_Boundary, World_Pos, World_Rot_Axis, 0.0, World_Scale, Raylib.GRAY);
      Raylib.DrawModelEx (Inner_Boundary, World_Pos, World_Rot_Axis, 0.0, World_Scale, Raylib.GRAY);

      declare
         Pos : Raylib.Vector3 := (0.0, 0.0, 0.0);
         Rot_Axis : Raylib.Vector3 := (0.0, 1.0, 0.0);
         Scale : Raylib.Vector3 := (0.1, 0.1, 0.1);
      begin
         Dt := Raylib.GetFrameTime;
         Car_Rot_Angle := Lerp (Car_Rot_Angle, 360.0, Dt);
         Raylib.DrawModelEx (Car, Pos, Rot_Axis, Car_Rot_Angle, Scale, Raylib.WHITE);
      end;

      Raylib.DrawGrid (10, 1.0);

      Raylib.EndMode3D;

      Raylib.EndDrawing;
   end loop;
   Raylib.CloseWindow;
end Raylib_Gym;