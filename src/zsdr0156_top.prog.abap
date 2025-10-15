
TABLES: zsdt0225,zsde0407_header.

CLASS:lcl_report_100 DEFINITION DEFERRED.

DATA: lo_report_100         TYPE REF TO lcl_report_100,
      it_screen_status      TYPE TABLE OF sy-ucomm,
      p_erro                TYPE bool,
      p_processo            TYPE char10,
      p_edit                TYPE bool,
      p_cedit               TYPE i,
      lv_campos_preenchidos TYPE abap_bool VALUE abap_true,
      ls_variant            TYPE disvariant,
      tl_bdc                TYPE TABLE OF bdcdata,
      wl_bdc                TYPE bdcdata.

DATA: o_alv_01          TYPE REF TO cl_gui_alv_grid,
      it_fieldcat_01    TYPE lvc_t_fcat,
      it_f4_01          TYPE lvc_t_f4,
      it_saida_01       TYPE STANDARD TABLE OF zsde0407_alv INITIAL SIZE 0,
      wa_saida_01       TYPE zsde0407_alv,
      wa_ZLEST0055      TYPE zlest0055,
      main_container    TYPE REF TO cl_gui_custom_container,
      go_container_0200 TYPE REF TO cl_gui_custom_container,
      go_alv_0200       TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat_0200  TYPE lvc_t_fcat,
      gt_saida_0200     TYPE TABLE OF zsde_alv_nfps_fat,
      go_container_0300 TYPE REF TO cl_gui_custom_container,
      go_alv_0300       TYPE REF TO cl_gui_alv_grid,
      gt_fieldcat_0300  TYPE lvc_t_fcat,
      gt_saida_0300     TYPE TABLE OF zsde_alv_consulta_lote.

CONSTANTS: gv_status1000 TYPE sy-pfkey VALUE 'STATUS_1000'.

SELECTION-SCREEN BEGIN OF BLOCK part1 WITH FRAME TITLE TEXT-c01 .
  SELECT-OPTIONS:
  p_bukrs     FOR zsdt0225-bukrs        NO-EXTENSION NO INTERVALS," OBLIGATORY,
  p_werks     FOR zsdt0225-werks        NO-EXTENSION NO INTERVALS," OBLIGATORY,
  p_clien     FOR zsdt0225-cl_codigo    NO INTERVALS," OBLIGATORY,
  p_matnr     FOR zsdt0225-cod_material NO INTERVALS," OBLIGATORY,
  p_safra     FOR zsdt0225-safra        NO INTERVALS MATCHCODE OBJECT zyear," OBLIGATORY,
  p_idlot     FOR zsdt0225-id_seq       NO-EXTENSION NO INTERVALS," OBLIGATORY.
  p_perio     FOR zsdt0225-dt_fatura." OBLIGATORY,
SELECTION-SCREEN END OF BLOCK part1.

*SELECTION-SCREEN BEGIN OF BLOCK part2 WITH FRAME TITLE TEXT-C02 .
*  PARAMETERS: p_bsik  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsak  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsid  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro,
*              p_bsad  AS CHECKBOX DEFAULT abap_false USER-COMMAND filtro.
*
*SELECTION-SCREEN END OF BLOCK part2.

INITIALIZATION.
  "ls_layout-edit = abap_true.
  ls_variant-report = sy-repid.

AT SELECTION-SCREEN OUTPUT.

  it_screen_status = VALUE #( ( CONV sy-ucomm( '' ) ) ).

  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = gv_status1000
      p_program = sy-repid
    TABLES
      p_exclude = it_screen_status.


AT SELECTION-SCREEN.

  CLEAR: p_erro,p_processo.

  CASE sy-ucomm.
    WHEN 'BT_LOTE_NOVO' OR 'BT_LOTE_CONSULTA'.

      IF sy-ucomm = 'BT_LOTE_NOVO'.
        p_processo = 'NEW'.
        CALL SELECTION-SCREEN '0100'.
      ENDIF.

      IF sy-ucomm = 'BT_LOTE_CONSULTA'.
        IF p_bukrs IS INITIAL AND p_safra IS INITIAL AND p_bukrs IS INITIAL AND p_idlot IS INITIAL.
          p_erro = abap_true.
        ELSE.
          IF p_bukrs IS NOT INITIAL.
            IF p_safra IS NOT INITIAL OR p_idlot IS NOT INITIAL.
            ELSE.
              p_erro = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
        IF p_erro = abap_true.
          MESSAGE 'Necessário preencher Empresa e safra ou Empresa e Lote/Id Seq' TYPE 'E'.
          STOP.
        ELSE.
          p_processo = 'READ'.
        ENDIF.
        CALL SELECTION-SCREEN '0300'.
      ENDIF.

    WHEN 'BT_FATURAS'.

      IF p_bukrs IS INITIAL OR ( p_perio IS INITIAL AND p_werks IS INITIAL ) .
        MESSAGE 'Obrigatório preenchimento da Empresa e Período ou Empresa e Filial' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CALL SCREEN '0200'.

    WHEN 'BACK' OR 'CANCEL' OR 'EXIT'.
      "LEAVE PROGRAM.
      SET SCREEN 0.
      LEAVE SCREEN.

  ENDCASE.
