*&---------------------------------------------------------------------*
*&  Include           ZXMCBU01
*&---------------------------------------------------------------------*
*

break ABAP.

*{   DELETE         ECDK900567                                        1
*\BREAK-POINT.
*}   DELETE

*DATA: wl_setnr(25) VALUE 'ME21N_LIB_PED_CENTRO',
*      BEGIN OF t_SET_VALUES OCCURS 0.
*        INCLUDE STRUCTURE  SETLEAF.
*DATA: END OF t_set_values.
*RANGES: r_bwart for MSEG-BWART,
*        r_setname for SETLEAF-SETNAME,
*        r_uname for syst-uname.
*
*
*
*IF SY-TCODE = 'MIGO'.
*refresh r_setname.
*MOVE 'IEQ' TO R_SETNAME.
*MOVE 'MIGO_TP_MOV' TO R_SETNAME-LOW. APPEND R_SETNAME.
*MOVE 'MIGO_USUARIO' TO R_SETNAME-LOW. APPEND R_SETNAME.
*
* break abap.
*
*  refresh t_set_values.
*  SELECT * FROM SETLEAF into table t_set_values
*  where SETNAME IN R_setname.
*
*   IF not t_set_values[] is initial.
*     refresh r_bwart.
*     MOVE 'IEQ' to r_BWART.
*     LOOP AT t_set_values  WHERE SETNAME = 'MIGO_TP_MOV'.
*       MOVE t_set_values-valfrom to r_BWART-low.
*       append r_BWART.
*     ENDLOOP.
*     IF r_bwart[] is initial.
*       append r_bwart.
*     ENDIF.
*   ENDIF.
*
*   IF xmcmseg-bwart in r_bwart.
*    refresh t_set_values.
*
*     IF not t_set_values[] is initial.
*       refresh r_uname.
*       MOVE 'IEQ' to r_uname.
*       LOOP AT t_set_values where setname = 'MIGO_USUARIO'.
*         MOVE t_set_values-valfrom to r_uname-low.
*         append r_uname.
*       ENDLOOP.
*     ENDIF.
*
*     IF NOT ( SY-UNAME IN r_uname ).
*       message e398(00) with 'Usuário'
*                             SY-UNAME
*                             ' não autorizado a utilizar este'
*                             ' tipo de movimento...'.
*
*     ENDIF.
*
*   ENDIF.
*
*
*
*
*
*ENDIF.
