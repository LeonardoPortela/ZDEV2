 TYPES: BEGIN OF ty_saida,
           Bukrs  TYPE bsik-bukrs,
           gsber  TYPE bsik-gsber,
           lifnr  TYPE bsik-lifnr,
           name1  TYPE lfa1-name1,
           hkont  TYPE bsik-hkont,
           gkont  TYPE bsik-gkont,
           ebeln  TYPE bsik-ebeln,
           belnr  TYPE bsik-belnr,
           budat  TYPE bsik-budat,
           bldat  TYPE bsik-bldat,
           waers  TYPE bsik-waers,
           dmbtr  TYPE bsik-dmbtr,
           dmbe2  TYPE bsik-dmbe2,
           blart  TYPE bsik-blart,
           bschl  TYPE bsik-bschl,
           zlspr  TYPE bsik-zlspr,
           sgtxt  TYPE bsik-sgtxt,
         END OF ty_saida.

  TYPES: BEGIN OF ty_email,
           Bukrs         TYPE bsik-bukrs,
           gsber         TYPE bsik-gsber,
           email         TYPE char10,
           html          TYPE string,
           destinatarios TYPE string,
           lista         TYPE lvc_value,
*         gkont  TYPE bsik-gkont,
*         lifnr  TYPE bsik-lifnr,
*         name1  TYPE lfa1-name1,
*         ebeln  TYPE bsik-ebeln,
*         belnr  TYPE bsik-belnr,
*         budat  TYPE bsik-budat,
*         bldat  TYPE bsik-bldat,
*         rfccur TYPE bsik-rfccur,
*         dmbtr  TYPE bsik-dmbtr,
*         dmbe2  TYPE bsik-dmbe2,
*         blart  TYPE bsik-blart,
*         bschl  TYPE bsik-bschl,
*         zlspr  TYPE bsik-zlspr,
*         sgtxt  TYPE bsik-sgtxt,
*         hkont  TYPE bsik-hkont,
         END OF ty_email.


  DATA: o_alv               TYPE REF TO cl_salv_table,
        o_model             TYPE REF TO cl_salv_table,
        it_saida            TYPE STANDARD TABLE OF ty_saida INITIAL SIZE 0,
        wa_saida            TYPE ty_saida,
        it_mail             TYPE STANDARD TABLE OF ty_email INITIAL SIZE 0,
        wa_mail             TYPE ty_email,
        ls_table_conteudo29 TYPE string,
        ls_table_conteudo39 TYPE string,
        ls_table_header     TYPE string.

TABLES: bsik, sscrfields.

  DATA: it_screen_status TYPE TABLE OF sy-ucomm.


  SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-001 .
    SELECT-OPTIONS: p_bukrs  FOR bsik-bukrs NO INTERVALS. "OBLIGATORY
    SELECT-OPTIONS: p_gsber  FOR bsik-gsber NO INTERVALS. "OBLIGATORY
    SELECT-OPTIONS: p_lifnr  FOR bsik-lifnr NO INTERVALS. "OBLIGATORY
    SELECT-OPTIONS: p_blart  FOR bsik-blart NO INTERVALS."OBLIGATORY
    SELECT-OPTIONS: p_bschl  FOR bsik-bschl NO INTERVALS.
    SELECT-OPTIONS: p_hkont  FOR bsik-hkont NO INTERVALS.
    "SELECT-OPTIONS: p_email  FOR (string) NO INTERVALS VISIBLE LENGTH 150.
  SELECTION-SCREEN END OF BLOCK part1.

  SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-002.
    PARAMETERS: r1 RADIOBUTTON GROUP grp DEFAULT 'X', " Default selected
                r2 RADIOBUTTON GROUP grp.
  SELECTION-SCREEN END OF BLOCK part2.

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
**        IF p_bukrs IS INITIAL.
***          IF screen-name(7) = 'P_BUKRS'.
***            screen-required = '2'.
***            MODIFY SCREEN.
***          ENDIF.
**        ENDIF.
*      ENDLOOP.
**      IF p_bukrs IS INITIAL.
**        MESSAGE 'Empresa é Obrigatorio!' TYPE 'E'.
**        STOP.
**      ENDIF.
*
*      CALL SELECTION-SCREEN '0100'.
*
*    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
*      SET SCREEN 0.
*      LEAVE SCREEN.
*  ENDCASE.


  START-OF-SELECTION.


  PERFORM valida_execucao.
