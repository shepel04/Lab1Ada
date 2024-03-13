with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Generic_Array_Sort;

procedure Main is

   Number : constant Integer := 10;
   subtype Index is Integer range 1 .. Number;

   ---------------------------Set struct worker---------------------------------

   type Worker is record
      Id : Integer;
      Step : Integer;
      Time_To_Run : Time_Span;
      Index_Pointer : Index;
   end record;

   ---------------------------Set arrays----------------------------------------

   type Is_Run_Arr is array (Index) of Boolean with Volatile;

   IR : Is_Run_Arr;

   type W_Arr is array (Index range <>) of Worker;

   W : W_Arr (Index);

   ----------------------------Task part----------------------------------------

   task type Worker_Task is
      entry Start_Work (Init_Info : in Worker);
   end Worker_Task;

   task body Worker_Task is
      Info : Worker;

      Sum : Integer := 0;

      Prev_Member : Integer := 0;

      procedure Output_Statistics is
      begin
         Put_Line (
            "     Id:" & Info.Id'Img &
            "     Sum:" & Sum'Img &
            "     Step:" & Info.Step'Img &
            "     Time set:" & Duration'Image (To_Duration (Info.Time_To_Run))
         );
      end;
   begin
      accept Start_Work (Init_Info : in Worker) do
         Info.Id := Init_Info.Id;
         Info.Step := Init_Info.Step;
         Info.Time_To_Run := Init_Info.Time_To_Run;
         Info.Index_Pointer := Init_Info.Index_Pointer;
      end;

      loop
         Prev_Member := Prev_Member + Info.Step;
         Sum := Sum + Prev_Member;

         delay 0.5;

         exit when not IR (Info.Index_Pointer);
      end loop;

      Output_Statistics;
   end;

   type T_Arr is array (Index) of Worker_Task;

   T : T_Arr;

   ------------------------------Random generator-------------------------------

   subtype Random_Range is Integer range 1 .. 5;
   package R is new
     Ada.Numerics.Discrete_Random (Random_Range);
   use R;
   G : Generator;

   --------------------------------Compare&Sorting proc init--------------------

   function "<" (L, R : Worker) return Boolean is
      L_T_Float : Float := Float (To_Duration (L.Time_To_Run));
      R_T_Float : Float := Float (To_Duration (R.Time_To_Run));
   begin
      return L_T_Float < R_T_Float;
   end "<";

   procedure Sort is
     new Ada.Containers.Generic_Array_Sort (Index, Worker, W_Arr);

   --------------------------------Init IR_array--------------------------------

   procedure Init_IR_Arr (IR : in out Is_Run_Arr) is
   begin
      for I in IR'Range loop
         IR (I) := True;
      end loop;
   end Init_IR_Arr;

   --------------------------------Init W_array---------------------------------

   procedure Init_W_Arr (W : in out W_Arr) is
   begin
      for I in W'Range loop
         W (I) := (
            Id => Integer (I),
            Step => Random (G),
            Time_To_Run => Milliseconds((Random (G) + I) * 1000),
            Index_Pointer => I
         );
      end loop;
   end Init_W_Arr;

   -----------------------------------------------------------------------------

   Wait_Time_Total : Time_Span := Milliseconds (0);
   Wait_Time_Current : Time_Span := Milliseconds (0);

begin

   Init_IR_Arr (IR);

   Init_W_Arr (W);

   Sort (W);

   for I in W'Range loop
      T (I).Start_Work (W (I));
   end loop;

   for I in W'Range loop
      Wait_Time_Current := W (I).Time_To_Run - Wait_Time_Total;

      delay To_Duration (Wait_Time_Current);

      IR (W (I).Index_Pointer) := False;

      Wait_Time_Total := Wait_Time_Total + Wait_Time_Current;
   end loop;

end Main;
