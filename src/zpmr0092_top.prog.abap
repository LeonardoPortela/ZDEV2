
TABLES: t001.

TYPES: BEGIN OF ty_celltab,
         fieldname TYPE  lvc_fname,
         style     TYPE  lvc_style,
         style2    TYPE  lvc_style,
         style3    TYPE  lvc_style,
         style4    TYPE  lvc_style,
         maxlen    TYPE  int4,
       END OF ty_celltab.

TYPES: BEGIN OF  ty_zpmr0002_0.
         INCLUDE STRUCTURE zpmr0002.
TYPES:   aprocc  TYPE icon-name,
         celltab TYPE  lvc_t_styl,
       END OF  ty_zpmr0002_0.

TYPES: BEGIN OF  ty_zpmr0002_2.
         INCLUDE STRUCTURE zpmr0002.
TYPES:   action TYPE  char10,
       END OF  ty_zpmr0002_2.

DATA: it_screen_status TYPE TABLE OF sy-ucomm.

DATA: it_saida          TYPE STANDARD TABLE OF ty_zpmr0002_0 INITIAL SIZE 0,
      it_saida2         TYPE STANDARD TABLE OF ty_zpmr0002_2 INITIAL SIZE 0,
      it_zpmr0002       TYPE STANDARD TABLE OF zpmr0002 INITIAL SIZE 0,
      wa_saida          TYPE ty_zpmr0002_0,
      wa_saida2         TYPE ty_zpmr0002_2,
      wa_zpmr0002       TYPE zpmr0002,
      p_tipo            TYPE char1,
      lt_modify_celltab TYPE STANDARD TABLE OF ty_celltab INITIAL SIZE 0,
      o_alv             TYPE REF TO cl_salv_table.

*SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
*  SELECT-OPTIONS: p_bukrs   FOR t001-bukrs  NO-EXTENSION NO INTERVALS. "OBLIGATORY
*SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-002 .
*  PARAMETERS: r1 RADIOBUTTON GROUP rad1,
*              r2 RADIOBUTTON GROUP rad1, "DEFAULT 'X',
*              r3 RADIOBUTTON GROUP rad1.
*SELECTION-SCREEN END OF BLOCK part2.

*SELECTION-SCREEN BEGIN OF BLOCK part3 WITH FRAME TITLE TEXT-003 .
*  PARAMETERS: p_vriant LIKE tline-tdline VISIBLE LENGTH 25.
*SELECTION-SCREEN END OF BLOCK part3.

*AT SELECTION-SCREEN OUTPUT.
*
*  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).
*
*  IF sy-dynnr = 1000.
*
*    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
*      EXPORTING
*        p_status  = 'STATUS_1000'
*        p_program = sy-repid
*      TABLES
*        p_exclude = it_screen_status.
*
*  ENDIF.
*
*AT SELECTION-SCREEN.
*  CASE sy-ucomm.
*    WHEN 'ONLI'.
*      LOOP AT SCREEN.
*        IF p_bukrs IS INITIAL.
**          IF screen-name(7) = 'P_BUKRS'.
**            screen-required = '2'.
**            MODIFY SCREEN.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      IF p_bukrs IS INITIAL.
*        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
*        STOP.
*      ENDIF.
*
*      CALL SELECTION-SCREEN '0100'.
*
*    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
*      SET SCREEN 0.
*      LEAVE SCREEN.
*  ENDCASE.


START-OF-SELECTION.



  FREE: it_saida2,it_saida.

  CLEAR: p_tipo.
  IMPORT it_saida2 FROM MEMORY ID 'ZPMR0092'.
  "Importando do programa ZPMR0091 para ZPMR0092.
  IF it_saida2 IS NOT INITIAL.
    CLEAR:wa_saida.
    LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida2>).


      CASE <fs_saida2>-action.
        WHEN 'COPY'.
          P_tipo = 'C'.
        WHEN 'EDIT'.
          P_tipo = 'E'.
        WHEN 'INSERT'.
          P_tipo = 'I'.
        WHEN OTHERS.
      ENDCASE.


      IF p_tipo = 'E'.
        lt_modify_celltab = VALUE #(
    ( fieldname = 'PERMIT' style = '00080000' )
    ( fieldname = 'WAERS' style = '00080000' )
    ( fieldname = 'VALOR_DE' style = '00080000' )
    ( fieldname = 'VALOR_ATE' style = '00080000' )
    ( fieldname = 'USUA_SUBST' style = '00080000' )
    ( fieldname = 'DATA_LIM' style = '00080000' )
    ).
      ELSE.
        "AQUI INCLUI AS COLUNAS QUE PODEM SER EDITADAS
        lt_modify_celltab = VALUE #(
      ( fieldname = 'NIVEL' style = '00080000' )
      ( fieldname = 'APROVADOR' style = '00080000' )
      ( fieldname = 'PERMIT' style = '00080000' )
      ( fieldname = 'WAERS' style = '00080000' )
      ( fieldname = 'VALOR_DE' style = '00080000' )
      ( fieldname = 'VALOR_ATE' style = '00080000' )
      ( fieldname = 'USUA_SUBST' style = '00080000' )
      ( fieldname = 'DATA_LIM' style = '00080000' )
        ).
      ENDIF.

      MOVE-CORRESPONDING <fs_saida2> TO wa_saida.

      APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
      <_put_celltab>-celltab = lt_modify_celltab."lt_celltab.

    ENDLOOP.
    SORT it_saida ASCENDING.
    FREE MEMORY ID 'ZPMR0092'.

  ENDIF.



  CALL SELECTION-SCREEN '0100'.
