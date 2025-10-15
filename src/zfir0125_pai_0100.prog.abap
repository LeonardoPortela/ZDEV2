*&---------------------------------------------------------------------*
*& Include          ZFIR0125_PAI_0100
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
* PAI Module 0100
*----------------------------------------------------------------------*
MODULE pai_100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      PERFORM free_objects.
      LEAVE PROGRAM.
    WHEN 'EXEC'.
      PERFORM get_data.
      PERFORM refresh_alv.
    WHEN 'REFRESH'.
      PERFORM refresh_alv.
  ENDCASE.
ENDMODULE.
