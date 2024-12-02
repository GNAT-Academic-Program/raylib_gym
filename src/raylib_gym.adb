with Raylib;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

procedure Raylib_Gym is

   type Ray_Access is access all Raylib.Ray;
   type Model_Access is access all Raylib.Model;

   package IC renames Interfaces.C;

   subtype CFloat is Interfaces.C.C_float;

   function Lerp (start, stop, alpha : CFloat) return CFloat is
      (start + (stop - start) * alpha);

   DEG2RAD : constant CFloat := 3.14159265359 / 180.0;

   screenWidth  : constant := 1200;
   screenHeight : constant := 720;
   Cam : aliased Raylib.Camera3D;

   Car : Raylib.Model;
   Race_Track : Raylib.Model;

   Outer_Boundary_Ptr : Model_Access := new Raylib.Model;
   Inner_Boundary_Ptr : Model_Access := new Raylib.Model;

   World_Pos      : constant Raylib.Vector3 := (0.0, 0.0, 0.0);
   World_Rot_Axis : constant Raylib.Vector3 := (0.0, 1.0, 0.0);
   World_Scale    : constant Raylib.Vector3 := (1.0, 1.0, 1.0);

   Offset : constant Raylib.Vector3 := (0.0, -0.5, 0.0);

   Car_Rot_Angle : CFloat := 0.0;

   -- New car variables
   Car_Position  : Raylib.Vector3 := (0.0, 0.0, 0.0);
   Car_Speed     : CFloat := 0.2;
   Car_Rotation  : CFloat := 0.0;
   Car_Forward   : Raylib.Vector3 := (0.0, 0.0, 100.0);

   package Mesh_Ptr_Conv is new System.Address_To_Access_Conversions (Raylib.Mesh);

   -- Instantiate generic math functions for CFloat
   package CFloat_Math is new Ada.Numerics.Generic_Elementary_Functions (CFloat);
   use CFloat_Math;

   function Vector3Distance (v1, v2 : Raylib.Vector3) return CFloat is
      (Sqrt ((v2.x - v1.x)**2 + (v2.y - v1.y)**2 + (v2.z - v1.z)**2));

   function Vector3Add (v1, v2 : Raylib.Vector3) return Raylib.Vector3 is
      (x => v1.x + v2.x, y => v1.y + v2.y, z => v1.z + v2.z);

   function Vector3Scale (v : Raylib.Vector3; scalar : CFloat) return Raylib.Vector3 is
      (x => v.x * scalar, y => v.y * scalar, z => v.z * scalar);

   function Vector3Normalize (v : Raylib.Vector3) return Raylib.Vector3 is
      Length : CFloat := Sqrt (v.x**2 + v.y**2 + v.z**2);
   begin
      return (x => v.x / Length, y => v.y / Length, z => v.z / Length);
   end;

   
   function Check_Hit (Model_Ptr : Model_Access; Ray_Ptr : Ray_Access) return Integer
   with Import, Convention => C, External_Name => "check_hit";

begin
   Raylib.InitWindow (screenWidth, screenHeight, New_String ("Raylib Gym"));

   Car            := Raylib.LoadModel (New_String ("./data/race_car.gltf"));
   Race_Track     := Raylib.LoadModel (New_String ("./data/race_track.gltf"));
   Outer_Boundary_Ptr.all := Raylib.LoadModel (New_String ("./data/outer_boundary.gltf"));
   Inner_Boundary_Ptr.all := Raylib.LoadModel (New_String ("./data/inner_boundary.gltf"));

   Cam.position    := (5.0, 15.0, 40.0);
   Cam.target      := (0.0, 0.0, 0.0);
   Cam.up          := (0.0, 1.0, 0.0);
   Cam.fovy        := 45.0;
   Cam.projection  := Raylib.CAMERA_PERSPECTIVE;

   Raylib.SetTargetFPS (60);

   while not Raylib.WindowShouldClose loop
      declare
         Dt                 : CFloat := Raylib.GetFrameTime;
         Previous_Position  : constant Raylib.Vector3 := Car_Position;
         Collision_Hit      : Boolean := False;
         Collision_Distance : CFloat := 0.0;
      begin

         Raylib.BeginDrawing;

         Raylib.ClearBackground (Raylib.RAYWHITE);

         Raylib.BeginMode3D (Cam);

         -- Draw the track models
         Raylib.DrawModelEx (Outer_Boundary_Ptr.all, World_Pos, World_Rot_Axis, 0.0,
                             World_Scale, Raylib.GRAY);
         Raylib.DrawModelEx (Inner_Boundary_Ptr.all, (World_Pos.x, World_Pos.y - 0.5, World_Pos.z), World_Rot_Axis, 0.0,
                             World_Scale, Raylib.GRAY);

         -- Car movement controls
         if Raylib.IsKeyDown (Raylib.KEY_G) then
            Car_Rotation := Car_Rotation + 90.0 * Dt;
         end if;

         if Raylib.IsKeyDown (Raylib.KEY_J) then
            Car_Rotation := Car_Rotation - 90.0 * Dt;
         end if;

         -- Calculate forward direction
         Car_Forward.x := Sin (Car_Rotation * DEG2RAD);
         Car_Forward.z := Cos (Car_Rotation * DEG2RAD);

         -- Forward movement
         if Raylib.IsKeyDown (Raylib.KEY_Y) then
            Car_Position.x := Car_Position.x + Car_Speed * Car_Forward.x;
            Car_Position.z := Car_Position.z + Car_Speed * Car_Forward.z;
         end if;

         -- Backward movement
         if Raylib.IsKeyDown (Raylib.KEY_H) then
            Car_Position.x := Car_Position.x - Car_Speed * Car_Forward.x;
            Car_Position.z := Car_Position.z - Car_Speed * Car_Forward.z;
         end if;

         Car_Rot_Angle := Car_Rotation;

         declare
            Ray_Ptr : Ray_Access := new Raylib.Ray;
            Ray_End_Point   : Raylib.Vector3;
            Hit_Inner_Boundary_Distance : Integer := 0;
            Hit_Outer_Boundary_Distance : Integer := 0;
         begin
            Ray_Ptr.all :=  ((Car_Position.x, Car_Position.y + 0.3, Car_Position.z), Car_Forward);
            Ray_End_Point := Vector3Add (Ray_Ptr.all.position, Vector3Scale (Ray_Ptr.all.direction, 5.0));

            -- Check for collision with inner boundary
            Hit_Inner_Boundary_Distance := Check_Hit (Inner_Boundary_Ptr, Ray_Ptr);
            If Hit_Inner_Boundary_Distance /=0 then
               Put_Line ("Inner distance: " & Hit_Inner_Boundary_Distance'Image);
            end if;

            -- Check for collision with outer boundary
            Hit_Outer_Boundary_Distance := Check_Hit (Outer_Boundary_Ptr, Ray_Ptr);
            If Hit_Outer_Boundary_Distance /=0 then
               Put_Line ("Outer distance: " & Hit_Outer_Boundary_Distance'Image);
            end if;
         end;

         -- Draw the car model
         declare
            Rot_Axis : constant Raylib.Vector3 := (0.0, 1.0, 0.0);
            Scale    : constant Raylib.Vector3 := (0.2, 0.2, 0.2);  -- Adjusted to match World_Scale
         begin
            Raylib.DrawModelEx (Car, Car_Position, Rot_Axis, Car_Rot_Angle, Scale,
                                Raylib.WHITE);
         end;

         Raylib.DrawGrid (10, 1.0);

         Raylib.EndMode3D;

         -- Display collision distance
         if Collision_Hit then
            declare
               Distance_Text : String := "Collision Distance: " & CFloat'Image (Collision_Distance);
            begin
               Raylib.DrawText (New_String (Distance_Text), 10, 40, 20, Raylib.RED);
            end;
         end if;

         Raylib.EndDrawing;
      end;
   end loop;

   Raylib.CloseWindow;
end Raylib_Gym;
