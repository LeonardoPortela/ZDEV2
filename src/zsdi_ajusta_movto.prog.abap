*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi_ajusta_movto NO STANDARD PAGE HEADING MESSAGE-ID sd.

*----------------------------------------------------------------------*
* Type Pools
*----------------------------------------------------------------------*
TYPE-POOLS icon.

TABLES: likp, vbfa,
        lv50c,
        vbuk,
        zsdt0023.
* BBKO/Vagner Santos - Ínício da alteração - 02.10.2010
TABLES lfa1.
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_msn,
         nr_romaneio TYPE znr_romaneio,
         tp_msn      TYPE bapi_mtype,
         messagem    TYPE bapi_msg,
       END   OF type_msn,


       BEGIN OF type_zsdt0023,
         fornecimento  TYPE zsdt0023-vbeln,
         doc_distri    TYPE zsdt0023-vbelv,
         ch_referencia TYPE zsdt0023-ch_referencia,
         doc_mat_s     TYPE zsdt0023-mblnr_s,
         ano_doc_mat_s TYPE zsdt0023-mjahr_s,
         dt_saida      TYPE zsdt0023-dt_saida,
         hs_saida      TYPE zsdt0023-hs_saida,
       END OF type_zsdt0023,

       "CS2017000598 22.05.2017
       BEGIN OF ty_zsdt0001_item,
         posnr_rem TYPE lips-posnr,
         itm_lote  TYPE i.
         INCLUDE    STRUCTURE zsdt0001_item.
TYPES END OF ty_zsdt0001_item.
"Fim CS2017000598 22.05.2017

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0001   TYPE TABLE OF zsdt0001,
      t_zlest0002  TYPE TABLE OF zlest0002,
      ti_zlest0100 TYPE TABLE OF zlest0100  WITH HEADER LINE,
      wa_zlest0100 TYPE zlest0100,
      t_roma       TYPE TABLE OF zsdt0001,
      t_fcat       TYPE TABLE OF lvc_s_fcat,
      t_tool       TYPE ui_functions,
      t_msn        TYPE TABLE OF type_msn,
      sl_msn       TYPE type_msn,
      t_bdc        TYPE TABLE OF bdcdata,
      l_timestamp  TYPE timestampl,
      vg_vstel     TYPE vbap-vstel,
      vg_erdat     TYPE vbap-erdat,
      vg_texto     TYPE char1,
      vg_desc      TYPE char600,
      vl_ponteiro  TYPE zlest0100-cont,
      t_messtab    TYPE TABLE OF bdcmsgcoll.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
DATA: v_vtext               TYPE tvstt-vtext,
      v_name1               TYPE kna1-name1,
      v_bezei               TYPE tvakt-bezei,
      v_name1_ag            TYPE lfa1-name1,
      v_lifnr               TYPE lfa1-lifnr,

      v_nr_romaneio         TYPE zsdt0001-ch_referencia,
      vl_delivery_c         TYPE bapishpdelivnumb-deliv_numb,

      vl_matdocumentyear_r  TYPE bapi2017_gm_head_ret-doc_year,
      vl_mat_doc_r          TYPE bapi2017_gm_head_ret-mat_doc,
      item_text_nr_romaneio TYPE bapi2017_gm_item_create-item_text.


DATA: w_zsdt0023_temp TYPE zsdt0023_temp.

DATA: lw_parvw TYPE vbpa-parvw.
*----------------------------------------------------------------------*
*                               VARIÁVEIS                              *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_vbak              TYPE vbak,
      wa_vbak             TYPE vbak,
      sl_zmmt0074         TYPE zmmt0074,
      s_vbap              TYPE vbap,
      "CS2017000598 22.05.2017
      tg_vbap             TYPE TABLE OF vbap WITH HEADER LINE,
      t_zsdt0001_item     TYPE TABLE OF ty_zsdt0001_item WITH HEADER LINE,
      t_zsdt0001_item_grp TYPE TABLE OF ty_zsdt0001_item WITH HEADER LINE,
      "Fim CS2017000598 22.05.2017
      s_cont              TYPE REF TO cl_gui_custom_container,
      s_alv               TYPE REF TO cl_gui_alv_grid,
      s_layout            TYPE lvc_s_layo,
      v_user              TYPE tvarv_val,
      vl_inco1            TYPE vbkd-inco1,
      vl_lifnr            TYPE vbpa-lifnr,
      v_charg             TYPE vbpok-charg,
      v_lgort             TYPE vbpok-lgort,
      l_erro              TYPE char01,
      t_sadrvb            TYPE TABLE OF sadrvb INITIAL SIZE 0 WITH HEADER LINE,
      t_vbpavb            TYPE TABLE OF vbpavb INITIAL SIZE 0 WITH HEADER LINE,

      wa_goodsmvt_header  TYPE bapi2017_gm_head_01,
      t_goodsmvt_item     TYPE TABLE OF bapi2017_gm_item_create,
      wa_goodsmvt_item    TYPE bapi2017_gm_item_create,
      t_return            TYPE TABLE OF bapiret2 WITH HEADER LINE,
      wa_return           TYPE bapiret2,
      wa_code             TYPE bapi2017_gm_code,
      wa_vbpavb           TYPE vbpavb,
      wa_depara           TYPE zsdt_depara_depo,
      wa_zmmt0017         TYPE zmmt0017. "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940


*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table  TYPE char10 VALUE 'T_ZSDT0001',
           c_04(2)  TYPE c VALUE '04',
           c_f50(3) TYPE c VALUE 'F50',
           c_z05(3) TYPE c VALUE 'Z05',
           c_zx1(3) TYPE c VALUE 'ZX1',
           c_05(2)  TYPE c VALUE '05',
           c_e      TYPE c VALUE 'E',
           c_x      TYPE c VALUE 'X'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
    SELECT-OPTIONS:
      p_vbeln FOR vbuk-vbeln NO INTERVALS NO-EXTENSION NO-DISPLAY MATCHCODE OBJECT vmva,
      p_remes FOR vbfa-vbeln NO INTERVALS NO-EXTENSION OBLIGATORY MATCHCODE OBJECT vmva.
* BBKO/Vagner Santos - Início da alteração - 02.10.2010
    SELECT-OPTIONS p_lifnr FOR lfa1-lifnr NO INTERVALS NO-DISPLAY NO-EXTENSION.
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010
    "parameters p_fat as checkbox default 'X'.
  SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.
PARAMETERS:  p_peso TYPE zsdt0001-peso_liq_pos_ret NO-DISPLAY.

*-#133089-21.02.2024-JT-inicio
PARAMETERS: p_fataut TYPE char01 NO-DISPLAY.
*-#133089-21.02.2024-JT-fim

AT SELECTION-SCREEN OUTPUT.
  IF  NOT ( lw_parvw IS INITIAL ).
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN: 'P_LIFNR-LOW'.
          screen-input     = '0'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ELSE.
    LOOP AT SCREEN.
      CASE screen-name.
        WHEN: 'P_LIFNR-LOW'.
          screen-input     = '1'.
      ENDCASE.
      MODIFY SCREEN.
    ENDLOOP.

  ENDIF.

AT SELECTION-SCREEN ON p_vbeln.

  CLEAR vl_inco1.
  SELECT SINGLE inco1
    FROM vbkd
    INTO (vl_inco1)
  WHERE  vbeln IN p_vbeln
  AND  posnr NE 0.

  IF ( vl_inco1 = 'CIF' ) AND ( NOT vl_inco1 IS INITIAL ).

    lw_parvw = 'SP'.

    SELECT SINGLE lifnr
        FROM vbpa
        INTO (p_lifnr-low)
        WHERE vbeln  IN p_vbeln
    AND   parvw  EQ lw_parvw.

  ENDIF.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

*jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj
*  DATA: lc_vbfa  TYPE vbfa.

*  SELECT SINGLE *
*    FROM vbfa
*    INTO lc_vbfa
*   WHERE vbeln = p_remes-low
*     AND vbtyp_n = 'J'
*     AND vbtyp_v = 'C'.

  SELECT SINGLE *
    FROM zsdt0023
    INTO @DATA(_0023)
  WHERE vbeln = @p_remes-low.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'Remessa nao Encontrada!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  p_vbeln-sign = 'I'.
  p_vbeln-option = 'EQ'.
  p_vbeln-low = _0023-vbelv.
  APPEND p_vbeln.

  lw_parvw = 'SP'.

  SELECT SINGLE lifnr
    FROM vbpa
    INTO p_lifnr-low
   WHERE vbeln  IN p_vbeln
     AND parvw  EQ lw_parvw.

*jjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjjj

  CLEAR: vl_delivery_c, v_nr_romaneio, vg_faturamento_autom. "*-#133089-21.02.2024-JT
  IF sy-tcode = 'ZLES0106' OR
     sy-tcode = 'ZLES0115' OR
     sy-tcode = 'ZLES0136' OR
     sy-tcode = 'ZMM0127'  OR
     sy-batch = abap_true  OR
     p_fataut = abap_true.   "*-#133089-21.02.2024-JT
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD v_nr_romaneio.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
  ENDIF.

*-#133089-21.02.2024-JT-inicio
*-----------------------------------------------
*-verifica se é faturamento automatico
*-----------------------------------------------
  IF p_fataut = abap_true.
*   SELECT SINGLE ch_referencia
*     INTO @DATA(_ch_ref)
*     FROM zlest0241
*    WHERE ch_referencia = @v_nr_romaneio
*      AND cancelado     = @abap_off.
*   IF sy-subrc = 0.
*     vg_faturamento_autom = abap_true.
*   ENDIF.
    vg_faturamento_autom = p_fataut.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  TRY.  "*-#133089-21.02.2024-JT-inicio
* Verifica OV
      PERFORM: z_verifica_ov   ,
* Verifica Romaneio
               z_verifica_rom  ,
*BBKO/Vagner Santos - Início da alteração - 02.10.2010
               z_verifica_agente_frete,
*BBKO/Vagner Santos - Fim da alteração - 02.10.2010
* Verifica Usuário
               z_seleciona_tvarvc,
* Monta FieldCat
               z_monta_fieldcat.
    CATCH zcx_error. "*-#133089-21.02.2024-JT-inicio
      RETURN.        "*-#133089-21.02.2024-JT-inicio
  ENDTRY.            "*-#133089-21.02.2024-JT-inicio

  IF v_nr_romaneio IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    TRY.    "*-#133089-21.02.2024-JT-inicio
        PERFORM z_handle_command USING 'REMESSA'.
      CATCH zcx_error. "*-#133089-21.02.2024-JT-inicio
        RETURN.        "*-#133089-21.02.2024-JT-inicio
    ENDTRY.            "*-#133089-21.02.2024-JT-inicio
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_OV                                            *
*&---------------------------------------------------------------------*
*                                Verifica OV                           *
*----------------------------------------------------------------------*
FORM z_verifica_ov.

  DATA: lw_lfa1  TYPE lfa1,
        lw_lifnr TYPE lfa1-lifnr.

  CLEAR vl_inco1.
  SELECT SINGLE inco1
    FROM vbkd
    INTO (vl_inco1)
  WHERE  vbeln IN p_vbeln
  AND  posnr NE 0.

  IF ( vl_inco1 = 'CIF' AND NOT vl_inco1 IS INITIAL ) AND ( sy-tcode EQ 'ZOPUS' ).
    SELECT SINGLE lifnr
        FROM vbpa
        INTO p_lifnr-low
      WHERE  vbeln IN p_vbeln
    AND    parvw  = 'SP'.
  ENDIF.

  CLEAR: lw_lifnr, lw_lfa1.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_lifnr-low
    IMPORTING
      output = lw_lifnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lw_lifnr
    IMPORTING
      output = lw_lifnr.

  SELECT SINGLE * FROM lfa1
    INTO lw_lfa1
  WHERE lifnr EQ lw_lifnr.

  v_lifnr    = lw_lfa1-lifnr.
  v_name1_ag = lw_lfa1-name1.



  SELECT SINGLE vbeln
    FROM vbap
    INTO p_vbeln
  WHERE  vbeln IN p_vbeln.


  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-002.
    LEAVE LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_OV

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_ROM                                           *
*&---------------------------------------------------------------------*
*                            Verifica Romaneio                         *
*----------------------------------------------------------------------*
FORM z_verifica_rom RAISING zcx_error "*-#133089-21.02.2024-JT-inicio
.

  DATA: tl_zsdt0001    TYPE TABLE OF zsdt0001,
        wa_tl_zsdt0001 TYPE zsdt0001,
        vl_zlsch       TYPE vbkd-zlsch,
        vl_zterm       TYPE vbkd-zterm,
        tl_ttext       TYPE TABLE OF ttext,
        sl_t052        TYPE t052,
        sl_t052u       TYPE t052u,
        sl_ttext       TYPE ttext.

  REFRESH t_zsdt0001.
  CLEAR: s_vbak  ,
         s_vbap  ,
         "CS2017000598 22.05.2017
         tg_vbap[],
         wa_tl_zsdt0001,
         t_zsdt0001_item[],
         t_zsdt0001_item_grp[],
         "Fim CS2017000598 22.05.2017
         vg_vstel,
         vg_erdat,
         vg_texto.


  SELECT SINGLE *
    FROM vbak
    INTO s_vbak
  WHERE  vbeln IN p_vbeln.

*  IF ( s_vbak-kvgr1 NE 'NÃO').
*    IF p_lifnr IS INITIAL.
**-#133089-21.02.2024-JT-inicio
*      CASE vg_faturamento_autom.
*        WHEN abap_off.
*          MESSAGE e899 WITH TEXT-027.
*        WHEN abap_true.
*          MESSAGE e899 WITH TEXT-027 INTO DATA(l_mesg).
*          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*      ENDCASE.
**-#133089-21.02.2024-JT-fim
*    ENDIF.
*  ENDIF.

*JJJJJJJJJJJJJJJJJJJJJ
  SELECT SINGLE ch_referencia
    FROM zsdt0023
    INTO v_nr_romaneio
  WHERE vbeln = p_remes-low
    AND vbelv = p_vbeln-low.

  IF sy-subrc <> 0.
    l_erro = abap_true.
    MESSAGE s899 WITH 'Romaneio não encontrado!'  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0001
    INTO @DATA(_w001)
  WHERE ch_referencia = @v_nr_romaneio.

  IF sy-subrc <> 0.
    l_erro = abap_true.
    MESSAGE s899 WITH 'Romaneio não encontrado!'  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  IF p_remes-low <> _w001-doc_rem.
    l_erro = abap_true.
    MESSAGE s899 WITH 'Remessa informada nao encontrada ZSDT0001!'  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*JJJJJJJJJJJJJJJJJJJJJ

  IF v_nr_romaneio IS NOT INITIAL.
    SELECT SINGLE *
      FROM zsdt0001
      INTO wa_tl_zsdt0001
    WHERE ch_referencia = v_nr_romaneio.

    SELECT SINGLE * FROM vbap INTO s_vbap
     WHERE vbeln IN p_vbeln
       AND matnr EQ wa_tl_zsdt0001-matnr.

    IF ( wa_tl_zsdt0001-matnr IS INITIAL ). "CS2017000598 22.05.2017

      SELECT *
       FROM vbap INTO TABLE tg_vbap
      WHERE vbeln IN p_vbeln.

      "Itens Romaneio
      SELECT *
        FROM zsdt0001_item INTO CORRESPONDING FIELDS OF TABLE t_zsdt0001_item
      WHERE ch_referencia = wa_tl_zsdt0001-ch_referencia.

      LOOP AT t_zsdt0001_item.
        IF t_zsdt0001_item-part_lote IS INITIAL.
          t_zsdt0001_item-part_lote = t_zsdt0001_item-cd_item.
        ENDIF.
        MODIFY t_zsdt0001_item.
      ENDLOOP.

      t_zsdt0001_item_grp[] = t_zsdt0001_item[].

      SORT t_zsdt0001_item_grp BY part_lote.
      DELETE ADJACENT DUPLICATES FROM t_zsdt0001_item_grp COMPARING part_lote.

      IF ( t_zsdt0001_item_grp[] IS INITIAL ).
*-#133089-21.02.2024-JT-inicio
*        CASE vg_faturamento_autom.
*          WHEN abap_off.
*            MESSAGE e899 WITH 'Itens do Romaneio não encontrado!'.
*            EXIT.
*          WHEN abap_true.
*            MESSAGE e899 WITH 'Itens do Romaneio não encontrado!' INTO l_mesg.
*            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*            lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*        ENDCASE.
*-#133089-21.02.2024-JT-fim
      ENDIF.

    ENDIF.
    "Fim CS2017000598 22.05.2017

  ELSE.
    SELECT SINGLE *
      FROM vbap
      INTO s_vbap
    WHERE  vbeln IN p_vbeln.
  ENDIF.

  IF ( wa_tl_zsdt0001-id_interface NE '48' AND
       wa_tl_zsdt0001-id_interface NE '49' AND
       wa_tl_zsdt0001-id_interface NE '51' AND
       wa_tl_zsdt0001-id_interface NE '52' ). "CS2017000598 22.05.2017

*    IF NOT s_vbak-zpesagem EQ '01'.
**-#133089-21.02.2024-JT-inicio
*      CASE vg_faturamento_autom.
*        WHEN abap_off.
*          MESSAGE i836 WITH TEXT-019.
*          LEAVE LIST-PROCESSING.
*        WHEN abap_true.
*          MESSAGE i836 WITH TEXT-019 INTO l_mesg.
*          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*      ENDCASE.
**-#133089-21.02.2024-JT-fim
*    ENDIF.

  ENDIF. "CS2017000598 22.05.2017

  SELECT SINGLE zlsch zterm
    FROM vbkd
    INTO (vl_zlsch, vl_zterm)
  WHERE  vbeln IN p_vbeln
  AND  posnr NE 0.

  IF vl_zlsch EQ 'D'.
    vg_texto = 'X'.
    IF NOT vl_zterm IS INITIAL.
      SELECT SINGLE *
        FROM t052
        INTO sl_t052
      WHERE  zterm EQ vl_zterm.

      SELECT SINGLE *
        FROM t052u
        INTO sl_t052u
      WHERE  spras EQ sy-langu
      AND  zterm EQ vl_zterm.

      IF NOT sl_t052 IS INITIAL.
        IF sl_t052u-text1 IS INITIAL.
          CALL FUNCTION 'FI_TEXT_ZTERM'
            EXPORTING
              i_t052  = sl_t052
            TABLES
              t_ztext = tl_ttext.
          LOOP AT tl_ttext INTO sl_ttext.
            IF sy-tabix EQ 1.
              vg_desc = sl_ttext-text1.
            ELSE.
              CONCATENATE vg_desc
                          sl_ttext-text1
                     INTO vg_desc SEPARATED BY space.
            ENDIF.
            CLEAR sl_ttext.
          ENDLOOP.
        ELSE.
          vg_desc = sl_t052u-text1.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*  IF s_vbak-auart EQ 'ZRAG'.
*    SELECT *
*       FROM zsdt0001
*       INTO TABLE t_zsdt0001
*     WHERE bukrs        EQ s_vbak-bukrs_vf
*       AND branch       EQ s_vbap-werks
*       AND nr_safra     EQ s_vbap-charg
*       AND parid        EQ s_vbak-kunnr
*       AND matnr        EQ s_vbap-matnr
*       AND tp_movimento EQ 'E'
*       AND status       NE 'X'.
*  ELSE.
*    IF ( wa_tl_zsdt0001-matnr IS INITIAL ) AND ( wa_tl_zsdt0001-ch_referencia IS NOT INITIAL ). "CS2017000598 22.05.2017
*      SELECT *
*        FROM zsdt0001
*        INTO TABLE t_zsdt0001
*      WHERE ch_referencia EQ wa_tl_zsdt0001-ch_referencia
*        AND vbeln         EQ p_vbeln-low
*        AND tp_movimento  EQ 'S'.
**        AND doc_rem       EQ space.
*      "AND status        NE 'X'.
*
*    ELSE.
*      SELECT *
*        FROM zsdt0001
*        INTO TABLE t_zsdt0001
*      WHERE vbeln  EQ s_vbap-vbeln
*        AND matnr  EQ s_vbap-matnr
*        AND tp_movimento EQ 'S'.
**        AND doc_rem      EQ space.
*      "AND status NE 'X'.
*
*      IF ( t_zsdt0001[] IS INITIAL ). "Tratativa abaixo para Cenario Fertilizantes - Importado
*        SELECT SINGLE *
*          FROM zsdt0001 INTO @DATA(lwa_zsdt0001_check)
*         WHERE ch_referencia EQ @v_nr_romaneio.
*
*        IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt0001_check-tp_movimento = 'S' ) AND ( lwa_zsdt0001_check-seq_lcto IS NOT INITIAL ).
*          SELECT *
*            FROM zsdt0001 INTO TABLE t_zsdt0001
*          WHERE ch_referencia EQ v_nr_romaneio
*            AND vbeln         EQ s_vbap-vbeln
*            AND matnr         EQ s_vbap-matnr
*            AND tp_movimento  EQ 'S'.
*          "status  - Ja vai estar marcado como "X"
*        ENDIF.
*      ENDIF.
*    ENDIF.
*  ENDIF.

*jjjjjjjjjjjjjjjjjjjjjjjjjj
  SELECT *
    FROM zsdt0001
    INTO TABLE t_zsdt0001
  WHERE ch_referencia EQ v_nr_romaneio.
*jjjjjjjjjjjjjjjjjjjjjjjjjj

  IF t_zsdt0001[] IS INITIAL.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE i836 WITH TEXT-003.
        LEAVE LIST-PROCESSING.
      WHEN abap_true.
        MESSAGE i836 WITH TEXT-003 INTO DATA(l_mesg).
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
        lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ENDIF.

  SORT t_zsdt0001 BY ch_referencia ASCENDING.
  vg_vstel = s_vbap-vstel.
  vg_erdat = s_vbap-erdat.

  SELECT SINGLE vtext
    FROM tvstt
    INTO v_vtext
  WHERE  spras EQ sy-langu
  AND  vstel EQ vg_vstel.

  SELECT SINGLE name1
    FROM kna1
    INTO v_name1
  WHERE  kunnr EQ s_vbak-kunnr.

  SELECT SINGLE bezei
    FROM tvakt
    INTO v_bezei
  WHERE  spras EQ sy-langu
  AND  auart EQ s_vbak-auart.

  tl_zsdt0001[] = t_zsdt0001[].
  SORT tl_zsdt0001 BY placa_cav ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zsdt0001 COMPARING placa_cav.
  DELETE tl_zsdt0001 WHERE placa_cav IS INITIAL.
  CHECK NOT tl_zsdt0001[] IS INITIAL.

  SELECT *
    FROM zlest0002
    INTO TABLE t_zlest0002
    FOR ALL ENTRIES IN tl_zsdt0001
  WHERE  pc_veiculo EQ tl_zsdt0001-placa_cav.

  SORT t_zlest0002 BY pc_veiculo ASCENDING.

ENDFORM.                    " Z_VERIFICA_ROM

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR                                         *
*&---------------------------------------------------------------------*
*                           Incluindo Botão ALV                        *
*----------------------------------------------------------------------*
FORM z_handle_toolbar USING p_object      TYPE REF TO cl_alv_event_toolbar_set
                            p_interactive TYPE char1.

* Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.

* Botão Vincular NF's
  CLEAR sl_toolbar.
  MOVE: 'REMESSA'          TO sl_toolbar-function ,
         icon_create       TO sl_toolbar-icon     ,
         TEXT-004          TO sl_toolbar-quickinfo,
         TEXT-004          TO sl_toolbar-text     ,
         space             TO sl_toolbar-disabled .
  APPEND sl_toolbar TO p_object->mt_toolbar.

ENDFORM.                    " Z_HANDLE_TOOLBAR

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND                                         *
*&---------------------------------------------------------------------*
*                      User Command Botões Incluidos                   *
*----------------------------------------------------------------------*
FORM z_handle_command USING p_ucomm TYPE syucomm
                      RAISING zcx_error. "*-#133089-21.02.2024-JT-inicio

  CHECK l_erro = abap_false.

  CASE p_ucomm.
    WHEN 'REMESSA'.
      " Inicio verificação Camila Brand
      " Verificar o picking.
      DATA: tg_0023         TYPE TABLE OF zmm0023,
            wa_0023         TYPE zmm0023,
            wa_0023_aux     TYPE sy-subrc,
            "VL_CENTRO_A     TYPE WERKS_D,
            vl_clabs_f      TYPE labst,
            vl_clabs_a      TYPE labst,
            vl_clabs_e      TYPE labst,
            vl_total        TYPE labst,
            vl_aux          TYPE char18,
            vl_msn1         TYPE char50,
            vl_msn2         TYPE char50,
            wa_tl_zsdt0001  TYPE zsdt0001,
            v_safra_a       TYPE zsdt0001-nr_safra,
            wa_setleaf      TYPE setleaf,
            it_setleaf      LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
            sl_zsdt0001_ent TYPE zsdt0001.

      SELECT *
      FROM zmm0023
      INTO TABLE tg_0023.

*      SORT tg_0023 BY  werks ASCENDING matnr ASCENDING cwerks DESCENDING. "CS2016001304
      SORT tg_0023 BY  werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING."PBALVES

      IF v_nr_romaneio IS NOT INITIAL.
        DELETE t_zsdt0001 WHERE   ch_referencia NE v_nr_romaneio.
      ENDIF.

      READ TABLE t_zsdt0001 INTO wa_tl_zsdt0001 INDEX 1.

      IF wa_tl_zsdt0001-id_interface NE '48' AND
         wa_tl_zsdt0001-id_interface NE '49' AND
         wa_tl_zsdt0001-id_interface NE '51' AND
         wa_tl_zsdt0001-id_interface NE '52'.

        TRY .
            "Verificar Depósito da Ordem de Venda
            zcl_deposito=>zif_deposito~get_instance(
                )->get_deposito_material_filial(
                    EXPORTING
                      i_matnr      = wa_tl_zsdt0001-matnr    " Nº do material
                      i_tp_produto = CONV #( COND string( WHEN wa_tl_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )    " Tipo de Produto
                      i_bukrs      = wa_tl_zsdt0001-bukrs    " Empresa
                      i_branch     = wa_tl_zsdt0001-branch    " Local de negócios
                    IMPORTING
                      e_lgort          = DATA(e_lgort)    " Depósito
                      e_centro_a_fixar = DATA(e_centro_a_fixar)
                ).

          CATCH zcx_deposito INTO DATA(ex_deposito).    " .
*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                ex_deposito->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
              WHEN abap_true.
                MESSAGE ID ex_deposito->msgid TYPE 'S' NUMBER ex_deposito->msgno WITH ex_deposito->msgv1 ex_deposito->msgv2 ex_deposito->msgv3 ex_deposito->msgv4 INTO DATA(l_mesg).
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
                lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDTRY.
      ELSE.
        CLEAR: e_centro_a_fixar.
      ENDIF.

      CLEAR wa_0023.
      READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = wa_tl_zsdt0001-branch
                                               matnr = wa_tl_zsdt0001-matnr. "lê o primeiro - SMC CS2023000120 Urgente - Atualização tela de bloqueio
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA
      IF sy-subrc NE 0.
        SELECT SINGLE matkl INTO @DATA(_matkl) FROM mara WHERE matnr = @wa_tl_zsdt0001-matnr.
        READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = wa_tl_zsdt0001-branch
                                                 matkl = _matkl.
      ENDIF.

      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      CLEAR wa_0023_aux.
      IF
        sy-subrc NE 0.
        wa_0023_aux = sy-subrc.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

      " Se for centro virtual na OV não verifica

      IF ( NOT sy-subrc = 0 OR wa_0023-status NE 'A' ) AND ( wa_tl_zsdt0001-matnr IS NOT INITIAL ) AND ( s_vbap-werks = wa_tl_zsdt0001-branch ).

        "ALRS 180118
        SELECT SINGLE *
         FROM vbap
         INTO s_vbap
        WHERE vbeln IN p_vbeln
          AND matnr EQ wa_tl_zsdt0001-matnr.

        "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO

*
*        SELECT SINGLE * INTO @DATA(wa_zmmt0017)
*          FROM zmmt0017
*         WHERE matnr       EQ @wa_tl_zsdt0001-matnr
*           AND centro_fixo EQ @wa_tl_zsdt0001-branch.

        zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
             EXPORTING
               i_material        = wa_tl_zsdt0001-matnr
               i_centro_fixo     = wa_tl_zsdt0001-branch
              " I_EUDR            =
             IMPORTING
               e_single_depara            =  wa_zmmt0017
           ).


*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

        IF wa_zmmt0017 IS NOT INITIAL.

          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
          IF wa_0023_aux IS NOT INITIAL.
            MESSAGE e897(sd) WITH  'Falta parâmetros na ZMM0029. '
                                      'Favor entrar em contato com '
                                       'a área de controladoria e estoque. '.
          ENDIF.
          "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC


          SELECT SINGLE clabs
              FROM mchb
              INTO vl_clabs_f
            WHERE  matnr EQ wa_tl_zsdt0001-matnr
              AND  werks EQ wa_tl_zsdt0001-branch
              AND  lgort EQ s_vbap-lgort
              AND  charg EQ wa_tl_zsdt0001-nr_safra.

          IF NOT e_centro_a_fixar IS INITIAL.
            IF wa_tl_zsdt0001-nr_safra GT '2019'.
              CONCATENATE wa_tl_zsdt0001-nr_safra '_' wa_tl_zsdt0001-branch INTO v_safra_a.
            ELSE.
              v_safra_a = wa_tl_zsdt0001-nr_safra.
            ENDIF.
            SELECT SINGLE clabs
              FROM mchb
              INTO vl_clabs_a
             WHERE matnr EQ wa_tl_zsdt0001-matnr
               AND werks EQ e_centro_a_fixar
               AND lgort EQ s_vbap-lgort
               AND charg EQ v_safra_a.
          ENDIF.

          "Checa se vai ter entrada de residuo
          "ALRS C
          CLEAR vl_clabs_e.

          "Conversao S4 Hana 27-07-23
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = wa_tl_zsdt0001-matnr
*            IMPORTING
*              output = wa_tl_zsdt0001-matnr.
          "Conversao S4 Hana 27-07-23


          DATA(_bukrs_mat_exc) = abap_false.
          PERFORM f_check_excecao_residuo USING wa_tl_zsdt0001-matnr
                                                wa_tl_zsdt0001-bukrs
                                       CHANGING _bukrs_mat_exc.

          SELECT * INTO TABLE it_setleaf
             FROM setleaf
             WHERE setname EQ 'RESIDUO'
             AND valfrom EQ wa_tl_zsdt0001-matnr.

          IF ( sy-subrc = 0 ) AND ( _bukrs_mat_exc EQ abap_false  ) .
            SELECT SINGLE *
              FROM zmmt0074
              INTO sl_zmmt0074
              WHERE werks = wa_tl_zsdt0001-branch
            AND   matnr = wa_tl_zsdt0001-matnr.

            IF sl_zmmt0074-entrada_rom = 'S'. "Checa romaneio de entrada
              IF wa_tl_zsdt0001-id_carga IS NOT INITIAL.

                CLEAR sl_zsdt0001_ent-peso_liq.

                SELECT SUM( peso_liq )
                  FROM zsdt0001
                  INTO sl_zsdt0001_ent-peso_liq
                 WHERE tp_movimento  EQ 'E'
                   AND id_carga      EQ wa_tl_zsdt0001-id_carga.

                IF sl_zsdt0001_ent-peso_liq IS INITIAL.
*-#133089-21.02.2024-JT-inicio
                  CASE vg_faturamento_autom.
                    WHEN abap_off.
                      MESSAGE e897(sd) WITH 'Não foram encontrados todos'
                                            'os romaneios de desmembramento!'.
                    WHEN abap_true.
                      MESSAGE e897(sd) WITH 'Não foram encontrados todos' 'os romaneios de desmembramento!' INTO l_mesg.
                      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
                      lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
                  ENDCASE.
*-#133089-21.02.2024-JT-fim
                ENDIF.

              ELSEIF wa_tl_zsdt0001-id_referencia IS NOT INITIAL.

                DATA(_nr_romaneio) = wa_tl_zsdt0001-id_referencia.

                SELECT SINGLE *
                  FROM zsdt0001
                  INTO sl_zsdt0001_ent
                  WHERE bukrs         = wa_tl_zsdt0001-bukrs
                  AND   branch        = wa_tl_zsdt0001-branch
                  AND   tp_movimento  = 'E'
                  AND   nr_romaneio   = _nr_romaneio
                  "AND   DT_MOVIMENTO  = WA_TL_ZSDT0001-DT_MOVIMENTO
                AND   nr_safra      = wa_tl_zsdt0001-nr_safra.

                IF sl_zsdt0001_ent-ch_refer_ent IS NOT INITIAL.

                  DATA(_peso_orig) = sl_zsdt0001_ent-peso_liq. "CS2017001903

                  CLEAR sl_zsdt0001_ent-peso_liq.
                  SELECT SUM( peso_liq )
                      FROM zsdt0001
                      INTO sl_zsdt0001_ent-peso_liq
                      WHERE bukrs         = sl_zsdt0001_ent-bukrs
                      AND   branch        = sl_zsdt0001_ent-branch
                      AND   tp_movimento  = 'E'
                      AND   ch_refer_ent  = sl_zsdt0001_ent-ch_refer_ent
                      "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
                  AND   nr_safra      = sl_zsdt0001_ent-nr_safra.


                  IF _peso_orig = sl_zsdt0001_ent-peso_liq.
*-#133089-21.02.2024-JT-inicio
                    CASE vg_faturamento_autom.
                      WHEN abap_off.
                        MESSAGE e897(sd) WITH 'Não foram encontrados todos'
                                              'os romaneios de desmembramento!'.
                      WHEN abap_true.
                        MESSAGE e897(sd) WITH 'Não foram encontrados todos' 'os romaneios de desmembramento!' INTO l_mesg.
                        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
                        lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
                    ENDCASE.
*-#133089-21.02.2024-JT-fim
                  ENDIF.

                ENDIF.
                IF sy-subrc =  0.
                  IF  wa_tl_zsdt0001-peso_liq GT sl_zsdt0001_ent-peso_liq.
                    vl_clabs_e    =  wa_tl_zsdt0001-peso_liq - sl_zsdt0001_ent-peso_liq.
                  ELSE.
                    "Não gera  doc. entrada
                  ENDIF.

                ENDIF.
              ENDIF.
            ELSE.

              vl_clabs_e    = wa_tl_zsdt0001-peso_liq.

            ENDIF.
          ENDIF.
          "ALRS C

          vl_total = vl_clabs_a + vl_clabs_f + vl_clabs_e.

          IF wa_tl_zsdt0001-peso_liq GT vl_total.
            vl_aux = vl_total.
            CONDENSE vl_aux NO-GAPS.
            CONCATENATE 'O total' vl_aux 'do centro' INTO vl_msn1 SEPARATED BY space.
            CONCATENATE wa_tl_zsdt0001-branch 'e' e_centro_a_fixar INTO vl_msn2 SEPARATED BY space.

*-#133089-21.02.2024-JT-inicio
            CASE vg_faturamento_autom.
              WHEN abap_off.
                MESSAGE e897(sd) WITH vl_msn1 vl_msn2 'é menor que a quantidade do picking'.
              WHEN abap_true.
                MESSAGE e897(sd) WITH vl_msn1 vl_msn2 'é menor que a quantidade do picking' INTO l_mesg.
                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
                lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.
        ENDIF.
      ENDIF.
      " Fim verificação Camila Brand
      " Fim verifica picking

*     Gera Remessa
      TRY.  "*-#133089-21.02.2024-JT-inicio
          PERFORM z_gera_remessa.
        CATCH zcx_error INTO DATA(ex_error). "*-#133089-21.02.2024-JT-inicio
          MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO l_mesg.
          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ). "*-#133089-21.02.2024-JT-inicio
      ENDTRY.            "*-#133089-21.02.2024-JT-inicio

      IF v_nr_romaneio IS INITIAL AND
         vg_faturamento_autom = abap_off.  "*-#133089-21.02.2024-JT-inicio
        CALL METHOD s_alv->refresh_table_display.
      ENDIF.

  ENDCASE.

ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&      Form  Z_GERA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                              Gera Remessa                            *
*----------------------------------------------------------------------*
FORM z_gera_remessa RAISING zcx_error. "*-#133089-21.02.2024-JT-inicio.

  DATA: tl_exec_cogi   TYPE TABLE OF rgsb4 WITH HEADER LINE,
        tl_rows        TYPE lvc_t_row,
        sl_rows        TYPE lvc_s_row,
        sl_roma        TYPE zsdt0001,
        vg_lfimg       TYPE lfimg,
        vg_wmeng       TYPE wmeng,
        vg_roma        TYPE lfimg,
        vg_saldo       TYPE lfimg,
        ordem          TYPE afpo-aufnr,
        sl_zsdt0001    TYPE zsdt0001,
        vl_sld_checked TYPE c,
        wa_mska        TYPE mska.

  DATA:tl_fields TYPE TABLE OF sval WITH HEADER LINE,
       lv_return TYPE vbpok-charg,
       v_xchpf   TYPE mara-xchpf,
       wa_mchb   TYPE mchb.

  REFRESH: t_roma,
           t_msn ,
           t_msn .

* Verifica Seleção de Linhas
  IF v_nr_romaneio IS INITIAL.
    CALL METHOD s_alv->get_selected_rows
      IMPORTING
        et_index_rows = tl_rows.
  ELSE.
    DELETE t_zsdt0001 WHERE   ch_referencia NE v_nr_romaneio.
    CLEAR tl_rows.
    REFRESH tl_rows.
    IF t_zsdt0001[] IS NOT INITIAL.
      sl_rows-index = 1.
      APPEND sl_rows TO tl_rows.
    ENDIF.
  ENDIF.

  IF tl_rows[] IS INITIAL.
    MESSAGE i836 WITH TEXT-016.
    EXIT.
  ENDIF.

  LOOP AT tl_rows INTO sl_rows.
    READ TABLE t_zsdt0001 INTO sl_roma INDEX sl_rows-index.
    APPEND sl_roma TO t_roma.
*    CLEAR: SL_ROWS, SL_ROMA.
  ENDLOOP.

  vl_sld_checked = 'X'. "CS2017000598 22.05.2017

  IF ( sl_roma-matnr IS NOT INITIAL ). "CS2017000598 22.05.2017
    "Ler novamente o item OV por material ALRS
    SELECT SINGLE *
      FROM vbap
      INTO s_vbap
      WHERE  vbeln IN p_vbeln
    AND    matnr EQ sl_roma-matnr.

    IF  sy-subrc NE 0.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i836 WITH 'Item do romaneio não encontrado' vg_roma.
          EXIT.
        WHEN abap_true.
          MESSAGE i836 WITH 'Item do romaneio não encontrado' vg_roma INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.

*   Desmenbramento OPUS - Saldo
    SELECT SINGLE SUM( lfimg ) INTO vg_lfimg
      FROM lips
     WHERE vgbel EQ s_vbap-vbeln
    AND   matnr EQ s_vbap-matnr.

* Alteração - RIM-SKM-IR129749-06.03.23 - Inicio
    SELECT SINGLE SUM( wmeng ) INTO vg_wmeng
      FROM vbep
     WHERE vbeln EQ s_vbap-vbeln
    AND   posnr EQ s_vbap-posnr.

* *>
**AJUSTE NÃO NECESSÁRIO POIS TRATATIVA REALIZADA NA GERAÇÃO DA OV TRANSAÇÃO ZSDT0044 PARA QUE NA MESMA OV NÃO TENHA ITENS COM MESMO MATERIAL
*    SELECT SINGLE SUM( wmeng ) INTO vg_wmeng
*    FROM vbep AS vb
*     INNER JOIN vbap AS vp ON vp~vbeln = vb~vbeln AND  vp~posnr = vb~posnr
*    WHERE vb~vbeln = s_vbap-vbeln
*     AND  vp~matnr = s_vbap-matnr.
* Alteração - RIM-SKM-IR129749-06.03.23 - Fim

    vg_roma = 0.
    LOOP AT t_roma INTO sl_zsdt0001.
      IF sl_zsdt0001-qtde_remessa IS NOT INITIAL.
        vg_roma = vg_roma + sl_zsdt0001-qtde_remessa.
      ELSE.
        vg_roma = vg_roma + sl_zsdt0001-peso_liq.
      ENDIF.
    ENDLOOP.

    vg_saldo = vg_wmeng - vg_lfimg.
    vg_lfimg = vg_lfimg + vg_roma.

    IF vg_lfimg GT vg_wmeng.
*-#133089-21.02.2024-JT-inicio
*      CASE vg_faturamento_autom.
*        WHEN abap_off.
*          MESSAGE i836 WITH TEXT-023 TEXT-024.
*          MESSAGE i836 WITH 'Saldo Atual:' vg_saldo ' Volume Romaneio(s):' vg_roma.
*        WHEN abap_true.
*          l_mesg = 'Sem saldo forneceimento p/ romaneio(s)' && 'Verif. Op. Demembramento OPUS' && 'Saldo Atual:' && vg_saldo && ' Volume Romaneio(s):' && vg_roma.
*          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*      ENDCASE.
*-#133089-21.02.2024-JT-fim
*      CLEAR: vl_sld_checked.
    ELSEIF ( s_vbap-kwmeng <> s_vbap-ntgew ).

      IF NOT ( s_vbap-umziz > 0 AND s_vbap-umzin > 0 AND s_vbap-umziz NE s_vbap-umzin ). "Não realizar a validação com o faturamento possuir unidade de conversão ( Quantidade OV diferente Peso Liquido )
*-#133089-21.02.2024-JT-inicio
*        CASE vg_faturamento_autom.
*          WHEN abap_off.
*            MESSAGE i836 WITH 'Quantidade da Ordem diferente do peso liquido.'.
*            MESSAGE i836 WITH 'Entre em contato com a área de execução.'.
*          WHEN abap_true.
*            l_mesg = 'Quantidade da Ordem diferente do peso liquido.' && 'Entre em contato com a área de execução.'.
*            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*            lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*        ENDCASE.
*-#133089-21.02.2024-JT-fim
*        CLEAR: vl_sld_checked.
      ENDIF.

    ENDIF.

  ENDIF.

*jjjjjjjjjjjjjjjjj
  vl_sld_checked = abap_true.
*jjjjjjjjjjjjjjjjj

  IF vl_sld_checked IS NOT INITIAL. "CS2017000598 22.05.2017

    IF ( sl_roma-matnr IS NOT INITIAL ). "CS2017000598 22.05.2017
      "Cria Remessa
*      MESSAGE s836 WITH 'Saldo Atual:' vg_saldo ' Volume Romaneio(s):' vg_roma.
    ENDIF.

    v_charg  = sl_roma-nr_safra.
    v_lgort  = s_vbap-lgort. "'ARMZ' 180118.

    IF ( s_vbap-charg IS INITIAL ) AND ( sl_roma-id_interface NE '48' AND
                                         sl_roma-id_interface NE '49' AND
                                         sl_roma-id_interface NE '51' AND
                                         sl_roma-id_interface NE '52' ). "CS2017000598 22.05.2017
      SELECT SINGLE xchpf
        INTO v_xchpf
        FROM mara
      WHERE matnr = s_vbap-matnr.
      IF v_xchpf = 'X'.

*-#133089-21.02.2024-JT-inicio
        CASE vg_faturamento_autom.
          WHEN abap_off.
            CLEAR: tl_fields, tl_fields[].
            tl_fields-tabname    = 'VBPOK'.
            tl_fields-fieldname  = 'CHARG'.
            tl_fields-field_obl  = 'X'.
            APPEND tl_fields.

            CALL FUNCTION 'POPUP_GET_VALUES'
              EXPORTING
                popup_title     = 'Informe o Lote da Remessa'
              IMPORTING
                returncode      = lv_return
              TABLES
                fields          = tl_fields
              EXCEPTIONS
                error_in_fields = 1
                OTHERS          = 2.
            IF sy-subrc IS INITIAL.
              v_charg  = tl_fields-value.
            ENDIF.
          WHEN abap_true.
            v_charg = sy-datum(4).
        ENDCASE.
*-#133089-21.02.2024-JT-fim

        PERFORM z_check_baixa.

        SELECT SINGLE *
          FROM mchb
          INTO wa_mchb
          WHERE matnr = s_vbap-matnr
          AND   werks = s_vbap-werks
          AND   lgort = s_vbap-lgort
          AND   charg = v_charg.

        IF sy-subrc NE 0.
          SELECT SINGLE *
          FROM mska
          INTO wa_mska
          WHERE matnr = s_vbap-matnr
          AND   werks = s_vbap-werks
          AND   lgort = s_vbap-lgort
          AND   charg = v_charg.

          IF sy-subrc NE 0.
            IF wa_mska-kalab LE 0. sy-subrc = 4.ENDIF.
          ENDIF.
        ENDIF.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM mska
            INTO @DATA(_mska)
             WHERE matnr  = @s_vbap-matnr
              AND   werks = @s_vbap-werks
              AND   lgort = @s_vbap-lgort
              AND   charg = @v_charg
              AND   vbeln = @s_vbap-vbeln.
          IF _mska-kalab NE sl_roma-peso_liq.
*-#133089-21.02.2024-JT-inicio
*            CASE vg_faturamento_autom.
*              WHEN abap_off.
*                MESSAGE 'Peso do romaneio difere do peso produzido,ajustar produção!' TYPE 'I'.
*                EXIT.
*              WHEN abap_true.
*                l_mesg = 'Peso do romaneio difere do peso produzido,ajustar produção!'.
*                lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*                lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*            ENDCASE.
*-#133089-21.02.2024-JT-fim
          ENDIF.

          v_lgort = s_vbap-lgort.

          CALL FUNCTION 'G_SET_GET_ALL_VALUES'
            EXPORTING
              client        = sy-mandt
              setnr         = 'WERKS_RUN_COGI'
              class         = '0000'
            TABLES
              set_values    = tl_exec_cogi
            EXCEPTIONS
              set_not_found = 1
              OTHERS        = 2.

          TRY.
              tl_exec_cogi = tl_exec_cogi[ field = 'WERKS' from = s_vbap-werks ].

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = v_charg
                IMPORTING
                  output = ordem.

              SELECT SINGLE *
                FROM afpo
                INTO @DATA(_order_item)
              WHERE aufnr = @ordem.

              IF ( _order_item-wemng IS INITIAL ).

                "//Forçar entrada de mercadoria
*                SUBMIT coruafwp EXPORTING LIST TO MEMORY
*                  WITH pafehl  EQ 'X'
*                  WITH pawerks = s_vbap-werks
*                  WITH palgort = s_vbap-lgort
*                  AND RETURN.

                CALL FUNCTION 'LIST_FREE_MEMORY'.

                "//Get cogi errors
                SELECT SINGLE *
                  FROM affw
                  INTO @DATA(_errors)
                WHERE charg = @v_charg.

                IF ( sy-subrc IS INITIAL ).
*                  SUBMIT coruaffw
*                    WITH s_werks  = s_vbap-werks
*                    WITH s_lgort  = s_vbap-lgort
*                    WITH s_matnr  = s_vbap-matnr
*                    WITH p_disply = abap_true
*                    AND RETURN.

                  EXIT.
                ENDIF.
              ENDIF.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ELSE.
*-#133089-21.02.2024-JT-inicio
*          CASE vg_faturamento_autom.
*            WHEN abap_off.
*              MESSAGE 'Lote Inválido!' TYPE 'I'.
*              EXIT.
*            WHEN abap_true.
*              l_mesg = 'Lote Inválido!'.
*              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*              lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.

      ENDIF.
    ENDIF.


    TRY.  "*-#133089-21.02.2024-JT-inicio
        PERFORM z_cria_remessa.
      CATCH zcx_error INTO DATA(ex_error). "*-#133089-21.02.2024-JT-inicio
        MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2 ex_error->msgv3 ex_error->msgv4 INTO l_mesg. ""*-#133089-21.02.2024-JT-inicio
        lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ). "*-#133089-21.02.2024-JT-inicio
    ENDTRY.            "*-#133089-21.02.2024-JT-inicio

  ENDIF.
ENDFORM.                    " Z_GERA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat,
           t_tool.

  DATA(lt_zsdt0001) = t_zsdt0001[].
  DELETE lt_zsdt0001 WHERE qtde_remessa IS INITIAL.
  DESCRIBE TABLE lt_zsdt0001 LINES DATA(lv_lines).

* Preenche FieldCat
  PERFORM z_preenche_fieldcat USING:
    c_table 'NR_ROMANEIO'  TEXT-005 12 space,
    c_table 'PARID'        TEXT-006 10 space,
    c_table 'BRANCH'       TEXT-007 06 space,
    c_table 'PLACA_CAV'    TEXT-008 07 space,
    c_table 'DT_MOVIMENTO' TEXT-009 10 space,
    c_table 'PESO_LIQ'     TEXT-010 16 space.

  IF lv_lines >= 1.
    PERFORM z_preenche_fieldcat USING:
      c_table 'QTDE_REMESSA' TEXT-030 07 space,
      c_table 'UM_REMESSA'   TEXT-031 07 space.
  ENDIF.

  PERFORM z_preenche_fieldcat USING:
    c_table 'TP_MOVIMENTO' TEXT-011 06 space,
    c_table 'MATNR'        TEXT-012 14 'X'  ,
    c_table 'TP_FRETE'     TEXT-013 07 space.


* Monta Layout
  PERFORM z_layout.

* Deleta Botões
  PERFORM z_deleta_bot USING: '&LOCAL&APPEND'       ,
                              '&LOCAL&COPY'         ,
                              '&LOCAL&COPY_ROW'     ,
                              '&LOCAL&CUT'          ,
                              '&LOCAL&DELETE_ROW'   ,
                              '&LOCAL&INSERT_ROW'   ,
                              '&LOCAL&MOVE_ROW'     ,
                              '&LOCAL&PASTE'        ,
                              '&LOCAL&PASTE_NEW_ROW',
                              '&LOCAL&UNDO'         ,
                              '&CHECK'              .

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_table TYPE c
                               p_field TYPE c
                               p_desc  TYPE c
                               p_len   TYPE n
                               p_zero  TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = p_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-no_zero   = p_zero.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  Z_DELETA_BOT                                             *
*&---------------------------------------------------------------------*
*                             Deleta Botões                            *
*----------------------------------------------------------------------*
FORM z_deleta_bot USING p_bot TYPE c.

  DATA sl_tool TYPE ui_func.

  sl_tool = p_bot.
  APPEND sl_tool TO t_tool.

ENDFORM.                    " Z_DELETA_BOT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

  IF vg_texto IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GP1'.
        screen-output    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF vg_desc IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GP2'.
        screen-output    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  CHECK s_cont IS INITIAL.

  CREATE OBJECT s_cont
    EXPORTING
      container_name = 'CC_ALV'.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-014.
  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  CHECK s_alv IS INITIAL.

  CREATE OBJECT s_alv
    EXPORTING
      i_parent = s_cont.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH TEXT-015.
  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER: s_event->zm_handle_user_command FOR s_alv,
               s_event->zm_handle_toolbar      FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA vl_int TYPE int4.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_default                     = 'X'
      is_layout                     = s_layout
      it_toolbar_excluding          = t_tool
    CHANGING
      it_outtab                     = t_zsdt0001
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  "CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                                Cria Remessa                          *
*----------------------------------------------------------------------*
FORM z_cria_remessa  RAISING zcx_error. "*-#133089-21.02.2024-JT-inicio..

  DATA: vl_delivery   TYPE bapishpdelivnumb-deliv_numb,
        v_xblnr       TYPE vbrk-xblnr,
        vrefer        TYPE zsdt0001-ch_referencia,
        tl_item       TYPE TABLE OF bapidlvreftosalesorder,
        sl_item       TYPE bapidlvreftosalesorder,
        tl_return     TYPE TABLE OF bapiret2,
        sl_zsdt0001   TYPE zsdt0001,
        sl_zlest0002  TYPE zlest0002,
        sl_data_rem   TYPE ledat,
        sl_ship_point TYPE vstel,
        wa_setleaf    TYPE setleaf,
        wl_erro(1),
        it_setleaf    LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
        et_vbkok      TYPE vbkok,
        vg_wadat      TYPE likp-wadat,
        vg_lfdat      TYPE likp-lfdat,
        w_msn         TYPE type_msn,
        msg_text      TYPE string,
        w_retent(1).

*jjjjjjjjjjjjjjjjjjjjjjjjjjjj
  SELECT SINGLE *
    FROM likp
    INTO @DATA(_likp)
   WHERE vbeln  IN @p_remes.

  IF sy-subrc <> 0.
    l_erro = abap_true.
    MESSAGE s024(sd) WITH 'Remessa nao encontrada!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  sl_data_rem = _likp-erdat.
*jjjjjjjjjjjjjjjjjjjjjjjjjjjj


  CLEAR: t_vbpavb[], t_sadrvb[], wa_goodsmvt_item, t_goodsmvt_item[], t_sadrvb[], t_vbpavb[], wa_vbpavb,t_return[], wa_depara.

  IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).

    CALL FUNCTION 'SD_PARTNER_READ'
      EXPORTING
        f_vbeln  = s_vbap-vbeln
        object   = 'VBPA'
      TABLES
        i_xvbadr = t_sadrvb
        i_xvbpa  = t_vbpavb.

    DELETE t_vbpavb WHERE parvw NE 'Z1'.

    IF NOT t_vbpavb[] IS INITIAL.

      READ TABLE t_roma INTO sl_zsdt0001 INDEX 1.
      READ TABLE t_vbpavb INTO wa_vbpavb INDEX 1.

*      SELECT SINGLE * INTO WA_DEPARA
*        FROM ZSDT_DEPARA_DEPO
*       WHERE WERKS EQ S_VBAP-VSTEL
*         AND LIFNR EQ WA_VBPAVB-LIFNR.

      DATA(_opera) = 'RF'.
      IF  s_vbak-auart EQ 'ZIND'.
        _opera = 'RI'.
      ENDIF.
*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255
*      CALL FUNCTION 'Z_BUSCA_DEPARA'
*        EXPORTING
*          i_werks          = s_vbap-vstel
*          i_lifnr          = wa_vbpavb-lifnr
*          i_opera          = _opera
*        IMPORTING
*          zsdt_depara_depo = wa_depara.
*
        ZCL_DEPARA_CENTRO_FIXO_VIRTUAL=>GET_DADOS_DEPARA(
            EXPORTING
              I_WERKS       = s_vbap-vstel
              I_LIFNR       = wa_vbpavb-lifnr
              I_OPERACAO    = _opera
            IMPORTING
             E_SINGLE_DEPARA          = wa_depara ).

*Parâmetros de Centro Real x Centro Virtual EUDR - BG #153255  - FIM


      IF wa_depara IS NOT INITIAL.
        wa_goodsmvt_item-move_plant = wa_depara-werks_v.
      ELSE.
        PERFORM: z_monta_erro_depara USING s_vbap-werks space.
      ENDIF.
    ENDIF.

  ENDIF.

  "ALRS
  CLEAR wl_erro.
  IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).
    LOOP AT t_roma INTO sl_zsdt0001.
      SELECT SINGLE *
        FROM vbak
        INTO wa_vbak
      WHERE vbeln = sl_zsdt0001-vbeln.
      IF ( wa_vbak-kvgr3 = 'C' AND sl_zsdt0001-tp_transgenia = 'RR' ) OR
         ( wa_vbak-kvgr3 = 'R' AND sl_zsdt0001-tp_transgenia = 'CO' ).

*-#133089-21.02.2024-JT-inicio
*        CASE vg_faturamento_autom.
*          WHEN abap_off.
*            MESSAGE s836(sd) WITH 'O Tipo de Produto informado no Romaneio :'
*                                  sl_zsdt0001-tp_transgenia
*                                  'esta diferente da Ordem d Venda :'
*                                  wa_vbak-kvgr3.
*          WHEN abap_true.
*            MESSAGE s836(sd) WITH 'O Tipo de Produto informado no Romaneio :' sl_zsdt0001-tp_transgenia 'esta diferente da Ordem d Venda :' wa_vbak-kvgr3 INTO DATA(l_mesg).
*            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*            lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*        ENDCASE.
**-#133089-21.02.2024-JT-fim
*        wl_erro = 'X'.
      ENDIF.
    ENDLOOP.
  ENDIF.
  CLEAR wa_zmmt0017.
  LOOP AT t_roma INTO sl_zsdt0001.
    "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO

*    SELECT SINGLE * INTO @DATA(wa_zmmt0017)
*      FROM zmmt0017
*     WHERE matnr       EQ @sl_zsdt0001-matnr
*       AND centro_fixo EQ @sl_zsdt0001-branch.

    zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
              EXPORTING
                i_material        = sl_zsdt0001-matnr
                i_centro_fixo     = sl_zsdt0001-branch
               " I_EUDR            =
              IMPORTING
                e_single_depara            = wa_zmmt0017
            ).

    " Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

    IF wa_zmmt0017 IS INITIAL.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM setleaf INTO @DATA(lwa_set_tp_ov_venda)
     WHERE setname EQ 'TIPO_OV_VENDA'
       AND valfrom EQ @s_vbak-auart.

    CHECK sy-subrc IS NOT INITIAL.

    IF sl_zsdt0001-id_interface EQ '48' OR
       sl_zsdt0001-id_interface EQ '49' OR
       sl_zsdt0001-id_interface EQ '51' OR
       sl_zsdt0001-id_interface EQ '52'.
      CONTINUE.
    ENDIF.

    TRY .
        "Verificar Depósito da Ordem de Venda
        zcl_deposito=>zif_deposito~get_instance(
            )->get_deposito_material_filial(
                EXPORTING
                  i_matnr      = sl_zsdt0001-matnr    " Nº do material
                  i_tp_produto = CONV #( COND string( WHEN sl_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )    " Tipo de Produto
                  i_bukrs      = sl_zsdt0001-bukrs    " Empresa
                  i_branch     = sl_zsdt0001-branch    " Local de negócios
                IMPORTING
                  e_lgort      = DATA(e_lgort)    " Depósito
            ).

      CATCH zcx_deposito INTO DATA(ex_deposito).    " .
*-#133089-21.02.2024-JT-inicio
*        CASE vg_faturamento_autom.
*          WHEN abap_off.
*            ex_deposito->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'S' ).
*          WHEN abap_true.
*            MESSAGE ID ex_deposito->msgid TYPE 'S' NUMBER ex_deposito->msgno WITH ex_deposito->msgv1 ex_deposito->msgv2 ex_deposito->msgv3 ex_deposito->msgv4 INTO l_mesg.
*            lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*            lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*        ENDCASE.
**-#133089-21.02.2024-JT-fim
*        wl_erro = 'X'.
    ENDTRY.

    SELECT SINGLE * FROM vbap INTO @DATA(wa_vbap) WHERE vbeln EQ @sl_zsdt0001-vbeln.
*    IF wa_vbap-lgort NE e_lgort.
**-#133089-21.02.2024-JT-inicio
*      CASE vg_faturamento_autom.
*        WHEN abap_off.
*          MESSAGE s011(zodvenda) WITH wa_vbap-lgort e_lgort.
*        WHEN abap_true.
*          MESSAGE s011(zodvenda) WITH wa_vbap-lgort e_lgort INTO l_mesg.
*          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
*          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*      ENDCASE.
**-#133089-21.02.2024-JT-fim
*      wl_erro = 'X'.
*    ENDIF.

  ENDLOOP.


  IF wl_erro = 'X'.
    EXIT.
  ENDIF.

  IF ( ( ( s_vbak-auart NE 'ZRDC' ) AND ( s_vbak-auart NE 'ZRFL' ) AND ( s_vbak-auart NE 'ZIND' ) ) AND ( wa_depara IS INITIAL ) ) OR
     ( ( ( s_vbak-auart EQ 'ZRDC' ) OR  ( s_vbak-auart EQ 'ZRFL' ) OR  ( s_vbak-auart EQ 'ZIND' ) ) AND ( NOT wa_depara IS INITIAL ) ).

    LOOP AT t_roma INTO sl_zsdt0001.

      CLEAR: it_setleaf[].

      "Conversao S4 Hana 27-07-23
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = sl_zsdt0001-matnr
*        IMPORTING
*          output = sl_zsdt0001-matnr.
      ""Conversao S4 Hana 27-07-23

      IF  s_vbap-werks =  sl_zsdt0001-branch. "Nao gera residuo centro virtual

        DATA(_bukrs_mat_exc) = abap_false.
        PERFORM f_check_excecao_residuo USING sl_zsdt0001-matnr
                                              sl_zsdt0001-bukrs
                                     CHANGING _bukrs_mat_exc.


        SELECT * INTO TABLE it_setleaf
           FROM setleaf
           WHERE setname EQ 'RESIDUO'
        AND valfrom EQ sl_zsdt0001-matnr.

        IF sy-subrc = 0 AND _bukrs_mat_exc EQ abap_false.
          SELECT SINGLE *
            FROM zmmt0074
            INTO sl_zmmt0074
            WHERE werks = s_vbap-werks
          AND   matnr = sl_zsdt0001-matnr.
          IF sy-subrc EQ 0.
            CLEAR w_retent.
*            PERFORM z_entrada_residuo USING sl_zsdt0001 vl_mat_doc_r vl_matdocumentyear_r w_retent.
*            IF w_retent = 'X'.
*              EXIT.
*            ENDIF.
          ELSE.
*            CONCATENATE 'Material obrigatório parâmetro Entrada Residuo, procure Depto Estoque' '!' INTO msg_text SEPARATED BY space.
*            w_msn-nr_romaneio = sl_zsdt0001-nr_romaneio.
*            w_msn-messagem = msg_text.
*            w_msn-tp_msn   = 'E'.
*            APPEND w_msn TO t_msn.
*            EXIT.
          ENDIF.
        ENDIF.

      ENDIF.


      REFRESH: tl_item, tl_return.

      CLEAR: vl_delivery, sl_item, sl_zlest0002.

      READ TABLE t_zlest0002 INTO sl_zlest0002
        WITH KEY pc_veiculo = sl_zsdt0001-placa_cav
        BINARY SEARCH.

      IF ( sl_zsdt0001-matnr IS INITIAL ). "CS2017000598 22.05.2017
        PERFORM z_add_itens_rom TABLES tl_item
                                 USING sl_zsdt0001.

        READ TABLE tg_vbap WITH KEY vbeln = p_vbeln-low.

        vg_erdat           = sl_zsdt0001-dt_movimento.
        sl_ship_point      = tg_vbap-vstel.

      ELSE.
        sl_item-ref_doc    = s_vbap-vbeln.
        sl_item-ref_item   = s_vbap-posnr.

        IF sl_zsdt0001-qtde_remessa IS NOT INITIAL.
          sl_item-dlv_qty    = sl_zsdt0001-qtde_remessa.
        ELSE.
          sl_item-dlv_qty    = sl_zsdt0001-peso_liq.
        ENDIF.

        IF p_peso GT 0.
          sl_item-dlv_qty  = p_peso.
        ENDIF.

        sl_item-sales_unit = s_vbap-vrkme.
        vg_erdat           = sl_zsdt0001-dt_movimento.
        sl_ship_point      = s_vbap-vstel.
        APPEND sl_item TO tl_item.
      ENDIF.

*      IF v_user IS INITIAL.
*        sl_data_rem = sy-datum.
*      ELSE.
*        sl_data_rem = sl_zsdt0001-dt_movimento.
*
*        IF sl_zsdt0001-fat_contingencia_ecc EQ abap_true.
*          DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.
*
*          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
*            EXPORTING
*              i_ch_referencia         = sl_zsdt0001-ch_referencia
*              i_get_dados_fat_ecc     = abap_true
*            IMPORTING
*              e_dados_faturamento_ecc = lwa_faturamento_ecc.
*
*          IF lwa_faturamento_ecc-data_lcto_nf IS INITIAL.
*            MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
*            RETURN.
*          ENDIF.
*
*          sl_data_rem = lwa_faturamento_ecc-data_lcto_nf.
*        ENDIF.
*
*        PERFORM memorizar_dt_movimento_badi USING sl_data_rem.
*      ENDIF.
*
      "Exporta variavel de romaneio para memoria e exit MV50AFZ1 importar
      vrefer = sl_zsdt0001-ch_referencia.
      EXPORT vrefer TO MEMORY ID 'MREFER'.
      "
      " bloqueio lote
      IF ( sl_zsdt0001-matnr IS NOT INITIAL ).
        CALL FUNCTION 'ENQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1      = 'E'
            mandt          = sy-mandt
            matnr          = sl_zsdt0001-matnr
            charg          = v_charg
            _scope         = '2'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc <> 0.
          vl_delivery_c = '9999999999'. "Erro bloqueio
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
          EXIT.
        ENDIF.
      ENDIF.
      "
*      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'
*        EXPORTING
*          ship_point        = sl_ship_point
*          due_date          = sl_data_rem
*        IMPORTING
*          delivery          = vl_delivery
*        TABLES
*          sales_order_items = tl_item
*          return            = tl_return.

*JJJJJJJJJJ
      vl_delivery = p_remes-low.
*JJJJJJJJJJ

      IF vl_delivery IS INITIAL.

*        PERFORM f_estorno_res  CHANGING sl_zsdt0001.
*
**     Retorna Erro
*        PERFORM z_monta_erro TABLES tl_return
*                              USING sl_zsdt0001.
      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        COMMIT WORK.
*        WAIT UP TO 2 SECONDS.
        "GRAVA REFERENCIA ROMANEIO
        v_xblnr = sl_zsdt0001-ch_referencia.
*        DO 100 TIMES.
*          SELECT SINGLE *
*            FROM likp
*            INTO @DATA(w_likp)
*            WHERE vbeln = @vl_delivery.
*          IF sy-subrc = 0.
*            UPDATE likp SET xblnr = v_xblnr
*            WHERE vbeln = vl_delivery.
*            COMMIT WORK.
*            EXIT.
*          ELSE.
*            WAIT UP TO 1 SECONDS.
*          ENDIF.
*        ENDDO.
        " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 1
*        IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).
*          CLEAR: zsdt0023.
*          zsdt0023-vbeln         = vl_delivery.
*          zsdt0023-vbelv         = s_vbap-vbeln.
*          zsdt0023-ch_referencia = sl_zsdt0001-ch_referencia.
*
*          zsdt0023-werks_v       = wa_depara-werks_v.
*          IF s_vbak-kvgr3 = 'C' AND wa_depara-lgort_t IS NOT INITIAL. "ALRS 04.05.2015
*            zsdt0023-lgort_v       = wa_depara-lgort_t.
*          ELSEIF s_vbak-kvgr3 = 'F' AND wa_depara-lgort_f IS NOT INITIAL. "ALRS 26.01.2018
*            zsdt0023-lgort_v       = wa_depara-lgort_f.
*          ELSE.
*            zsdt0023-lgort_v       = wa_depara-lgort.
*          ENDIF.
*          zsdt0023-dt_saida      = sy-datum.
*          zsdt0023-hs_saida      = sy-uzeit.
*          INSERT zsdt0023.
*          COMMIT WORK.
*        ENDIF.

*     Retorna Sucesso
*        PERFORM: z_monta_sucesso USING vl_delivery
*                                       sl_zsdt0001
*                                       space      ,
**     Salva Textos
*                 z_salva_texto   USING vl_delivery
*                                       sl_zsdt0001,
**BBKO/Vagner Santos - Início da alteração - 02.10.2010
*                 z_incluir_agente_frete USING vl_delivery
*                                              sl_zsdt0001,
*BBKO/Vagner Santos - Fim da alteração - 02.10.2010
*     Picking
        PERFORM:              z_picking USING vl_delivery
                                              sl_zsdt0001
                                              sl_data_rem.
*   Registrar SM
*               z_reg_sm        USING vl_delivery.
        DELETE t_zsdt0001 WHERE ch_referencia EQ sl_zsdt0001-ch_referencia.
        vl_delivery_c = vl_delivery.
        IF v_nr_romaneio IS NOT INITIAL AND sy-calld = 'X'.
          SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
        ENDIF.
      ENDIF.

      IF ( sl_zsdt0001-matnr IS NOT INITIAL ).
        CALL FUNCTION 'DEQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1 = 'E'
            mandt     = sy-mandt
            matnr     = sl_zsdt0001-matnr
            charg     = v_charg.
      ENDIF.


*      CLEAR SL_ZSDT0001.

    ENDLOOP.
  ENDIF.


*  IF NOT t_msn[] IS INITIAL.
*    IF v_nr_romaneio IS NOT INITIAL AND sy-calld = 'X'.
*      READ TABLE t_msn INTO sl_msn WITH KEY tp_msn = 'E'.
*      IF sy-subrc = 0.
*        REFRESH ti_zlest0100.
*        CLEAR vl_ponteiro.
*        SELECT  MAX( cont )
*              FROM zlest0100
*              INTO vl_ponteiro
*        WHERE ch_referencia = sl_zsdt0001-ch_referencia.
*
*        IF sy-subrc = 0.
*          ADD 1 TO vl_ponteiro.
*        ELSE.
*          vl_ponteiro = 1.
*        ENDIF.
*        LOOP AT t_msn INTO sl_msn.
*          wa_zlest0100-mandt      = sy-mandt.
*          wa_zlest0100-ch_referencia   = sl_zsdt0001-ch_referencia.
*          wa_zlest0100-msgtyp     = sl_msn-tp_msn.
*          wa_zlest0100-msgspra    = sy-langu.
*          wa_zlest0100-msgid      = 'OPUS'.
*          wa_zlest0100-msgnr      = '000'.
*          wa_zlest0100-msgv1      = sl_msn-messagem.
*          wa_zlest0100-data       = sy-datum.
*          wa_zlest0100-hora       = sy-uzeit.
*          wa_zlest0100-usuario    = sy-uname.
*          wa_zlest0100-cont       = vl_ponteiro.
*
*          APPEND wa_zlest0100 TO ti_zlest0100.
*          ADD 1 TO vl_ponteiro.
*        ENDLOOP.
*        IF ti_zlest0100[] IS NOT INITIAL.
*          MODIFY zlest0100 FROM TABLE ti_zlest0100.
*        ENDIF.
*        CLEAR vl_delivery_c.
*        SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
*
*        IF vg_faturamento_autom IS INITIAL.  "*-#133089-21.02.2024-JT-inicio
*          CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
*            TABLES
*              table    = t_msn
*            EXCEPTIONS
*              fb_error = 1
*              OTHERS   = 2.
*
*          IF NOT sy-subrc IS INITIAL.
*            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          ENDIF.
*        ENDIF.
*      ENDIF.
**      READ TABLE T_MSN INTO SL_MSN WITH KEY TP_MSN = 'S'.
**      IF SY-SUBRC = 0.
**        EXIT.
**      ENDIF.
*    ELSE.
*      IF vg_faturamento_autom IS INITIAL.  "*-#133089-21.02.2024-JT-inicio
*        CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
*          TABLES
*            table    = t_msn
*          EXCEPTIONS
*            fb_error = 1
*            OTHERS   = 2.
*
*        IF NOT sy-subrc IS INITIAL.
*          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*
**-#133089-21.02.2024-JT-inicio
*    CASE vg_faturamento_autom.
*      WHEN abap_off.
*      WHEN abap_true.
*        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_tab_erro = t_msn i_status = 'REME' ).
*        lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
*    ENDCASE.
**-#133089-21.02.2024-JT-fim
*  ENDIF.

ENDFORM.                    " Z_CRIA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                             Retorna Erro                             *
*----------------------------------------------------------------------*
FORM z_monta_erro TABLES p_return   STRUCTURE bapiret2
                   USING p_zsdt0001 TYPE zsdt0001.

  DATA: sl_return TYPE bapiret2.


  LOOP AT p_return INTO sl_return.
    sl_msn-nr_romaneio = p_zsdt0001-nr_romaneio.
    sl_msn-tp_msn      = sl_return-type.
    sl_msn-messagem    = sl_return-message.
    APPEND sl_msn TO t_msn.
    CLEAR: sl_return,
           sl_msn   .
  ENDLOOP.

ENDFORM.                    " Z_MONTA_ERRO
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_SUCESSO                                          *
*&---------------------------------------------------------------------*
*                            Retorna Sucesso                           *
*----------------------------------------------------------------------*
FORM z_monta_sucesso USING p_delivery TYPE vbeln_vl
                           p_zsdt0001 TYPE zsdt0001
                           p_rem      TYPE char1.

  DATA: sl_msn  TYPE type_msn,
        sl_text TYPE char50.

  sl_msn-nr_romaneio = p_zsdt0001-nr_romaneio.
  sl_msn-tp_msn      = 'S'.

  CASE p_rem.
    WHEN space.
      sl_text = TEXT-017.
    WHEN 'F'.
      sl_text = TEXT-029.
    WHEN 'T'.
      sl_text = TEXT-026.
  ENDCASE.


  IF p_zsdt0001 IS INITIAL.
    sl_text = TEXT-028.
  ENDIF.

  CONCATENATE sl_text
              p_delivery
         INTO sl_msn-messagem SEPARATED BY space.

  APPEND sl_msn TO t_msn.

ENDFORM.                    " Z_MONTA_SUCESSO

*&---------------------------------------------------------------------*
*&      Form  Z_PICKING                                                *
*&---------------------------------------------------------------------*
*                                  Picking                             *
*----------------------------------------------------------------------*
FORM z_picking USING p_delivery TYPE vbeln_vl
                     p_zsdt0001 TYPE zsdt0001
                     p_data_rem TYPE ledat.

  DATA: sl_vbkok_wa          TYPE vbkok,
        tl_vbpok             TYPE TABLE OF vbpok,
        tl_prot              TYPE TABLE OF prott,
        sl_vbpok             TYPE vbpok,
        sl_prot              TYPE prott,
        tl_return            TYPE bapiret2_t,
        sl_return            TYPE bapiret2,
        vl_msn               TYPE char200,
        vl_auart             TYPE zzauart,
        vl_tknum             TYPE vttk-tknum,
        vg_sysubrc           TYPE sy-subrc,
        vg_erro_mat_s        TYPE c LENGTH 1,
        vg_erro_mat_e        TYPE c LENGTH 1,
        vg_erro_pic          TYPE c LENGTH 1,
        v_peso_bruto         TYPE brgew_15,
        v_peso_liq           TYPE ntgew_15,
        vl_mat_doc_s         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear_s TYPE bapi2017_gm_head_ret-doc_year,
        vl_mat_doc_e         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear_e TYPE bapi2017_gm_head_ret-doc_year,
        vl_fat               TYPE vbak-vbeln,
        wa_setleaf           TYPE setleaf,
        it_setleaf           LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE.

  DATA: t_romaneios TYPE zsdt0001_t,
        v_faturar	  TYPE char01,
        v_mensagem  TYPE char255.

  DATA: c_r(2) TYPE c.

*JJJJJJJJJJJJJJJJJJJJJJJJJJJ
  GET TIME STAMP FIELD l_timestamp.

  w_zsdt0023_temp-mandt     = sy-mandt.
  w_zsdt0023_temp-vbeln     = p_remes-low.
  w_zsdt0023_temp-seq       = l_timestamp.
  w_zsdt0023_temp-user_reg  = sy-uname.
  w_zsdt0023_temp-data_reg  = sy-datum.
  w_zsdt0023_temp-hora_reg  = sy-uzeit.
  MODIFY zsdt0023_temp FROM w_zsdt0023_temp.
  COMMIT WORK.
*JJJJJJJJJJJJJJJJJJJJJJJJJJJ

  CLEAR: t_romaneios[], v_faturar, v_mensagem.

*  CALL METHOD zcl_romaneio=>get_ck_faturar
*    EXPORTING
*      i_ch_referencia_sai = p_zsdt0001-ch_referencia
*    IMPORTING
*      e_romaneios         = t_romaneios.

  "CS2017000598 22.05.2017
  IF s_vbak-auart = 'ZTER'.
    " Atualiza ZSDT0001
*    UPDATE zsdt0001
*       SET status  = 'X'
*           doc_rem = p_delivery
*     WHERE ch_referencia EQ p_zsdt0001-ch_referencia.
*
*    CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
*      EXPORTING
*        cd_referencia = p_zsdt0001-ch_referencia
*        tp_bloqueio   = 'X'.

    EXIT.
  ENDIF.
  "Fim CS2017000598 22.05.2017

  IF ( p_zsdt0001-matnr IS INITIAL ). "CS2017000598 22.05.2017
    PERFORM z_ins_lote_itens TABLES tl_return
                              USING p_delivery.

    IF tl_return[] IS NOT INITIAL.
      PERFORM z_monta_erro TABLES tl_return
                            USING p_zsdt0001.
    ENDIF.
  ELSE.
    IF s_vbap-charg IS INITIAL.
      PERFORM z_ins_lote USING p_delivery v_charg.
    ENDIF.
  ENDIF.

  "Determinação Peso Bruto/ Peso Liquido
  CLEAR: v_peso_bruto, v_peso_liq.

  REFRESH:  tl_return.

  IF p_peso GT 0.
    v_peso_bruto  = p_zsdt0001-peso_liq.
    v_peso_liq    = p_peso.

    PERFORM z_troca_peso       TABLES tl_return
                               USING p_delivery
                                     p_zsdt0001
                                     v_peso_bruto
                                     v_peso_liq.

    IF tl_return[] IS NOT INITIAL.
      PERFORM z_monta_erro TABLES tl_return
                            USING p_zsdt0001.
    ENDIF.
  ELSEIF ( p_zsdt0001-matnr IS NOT INITIAL ).
    READ TABLE t_romaneios INTO DATA(_wl_rom) WITH KEY ch_referencia = p_zsdt0001-ch_referencia.
    IF ( sy-subrc EQ 0 ) AND ( _wl_rom-peso_subtotal IS NOT INITIAL ) AND ( _wl_rom-peso_liq IS NOT INITIAL ).
      v_peso_bruto  = _wl_rom-peso_subtotal.
      v_peso_liq    = _wl_rom-peso_liq.

      PERFORM z_troca_peso       TABLES tl_return
                                 USING p_delivery
                                       p_zsdt0001
                                       v_peso_bruto
                                       v_peso_liq.

      IF tl_return[] IS NOT INITIAL.
        PERFORM z_monta_erro TABLES tl_return
                              USING p_zsdt0001.
      ENDIF.
    ENDIF.
  ENDIF.

  REFRESH: tl_vbpok ,
           tl_return.
  sl_vbkok_wa-vbeln_vl  = p_delivery.
  sl_vbkok_wa-vbeln     = p_delivery.
  sl_vbkok_wa-wabuc     = 'X'.
  sl_vbkok_wa-wadat_ist = p_data_rem.

  IF ( p_zsdt0001-matnr IS INITIAL ). "CS2017000598 22.05.2017

    LOOP AT t_zsdt0001_item WHERE ch_referencia = p_zsdt0001-ch_referencia.

      READ TABLE tg_vbap WITH KEY vbeln = t_zsdt0001_item-vbeln
                                  posnr = t_zsdt0001_item-posnr.

      sl_vbpok-vbeln_vl       = p_delivery.
      sl_vbpok-posnr_vl       = t_zsdt0001_item-itm_lote.
      sl_vbpok-vbeln          = p_delivery.
      sl_vbpok-posnn          = t_zsdt0001_item-posnr_rem.
      sl_vbpok-matnr          = t_zsdt0001_item-matnr.
      sl_vbpok-pikmg          = t_zsdt0001_item-lfimg.
      sl_vbpok-charg          = t_zsdt0001_item-charg.
      sl_vbpok-gewei          = 'KG'.

*    *************************************************
      sl_vbpok-lgort          = tg_vbap-lgort.
      sl_vbpok-brgew          = t_zsdt0001_item-brgew.
      sl_vbpok-ntgew          = t_zsdt0001_item-ntgew.
*    *************************************************
      APPEND sl_vbpok TO tl_vbpok.
    ENDLOOP.

  ELSE.

    sl_vbpok-vbeln_vl       = p_delivery.
    sl_vbpok-posnr_vl       = 10.
    sl_vbpok-vbeln          = p_delivery.
    sl_vbpok-posnn          = 10.
    sl_vbpok-matnr          = s_vbap-matnr.
    IF p_peso  GT 0.
      sl_vbpok-pikmg          = p_peso .
    ELSE.

      IF p_zsdt0001-qtde_remessa IS NOT INITIAL.
        sl_vbpok-pikmg          = p_zsdt0001-qtde_remessa.
      ELSE.
        sl_vbpok-pikmg          = p_zsdt0001-peso_liq.
      ENDIF.

    ENDIF.
    sl_vbpok-charg          = v_charg .
    sl_vbpok-gewei          = 'KG'.

*  *************************************************
    IF ( p_zsdt0001-id_interface EQ '48' OR
         p_zsdt0001-id_interface EQ '49' OR
         p_zsdt0001-id_interface EQ '51' OR
         p_zsdt0001-id_interface EQ '52' ). "CS2017000598 22.05.2017
      sl_vbpok-lgort        = s_vbap-lgort.
    ELSE.
      sl_vbpok-lgort        = v_lgort.
    ENDIF.

    IF v_peso_bruto GT 0.
      sl_vbpok-brgew          = v_peso_bruto.
      sl_vbpok-ntgew          = v_peso_liq.
    ELSE.
      sl_vbpok-brgew          = p_zsdt0001-peso_liq.
      sl_vbpok-ntgew          = p_zsdt0001-peso_liq.
    ENDIF.
*  *************************************************

    APPEND sl_vbpok TO tl_vbpok.

  ENDIF."CS2017000598 22.05.2017

  CLEAR : vl_mat_doc_s, vl_matdocumentyear_s, vl_mat_doc_e, vl_matdocumentyear_e.

  CLEAR: tl_prot[], vg_erro_mat_s, vg_erro_mat_e, vg_erro_pic, vg_sysubrc.
  CLEAR: wa_return.

  " Ajustes ZOPUS - Remessa e Armazenagem - CH: 86446.
  REFRESH: it_setleaf[].
  CLEAR: wa_setleaf.

  SELECT * INTO TABLE it_setleaf
    FROM setleaf
    WHERE setname EQ 'MAGGI_ARMAZENAGEM_VA01'
  AND valfrom EQ s_vbak-auart.

  CLEAR: c_r.
  IF  ( sy-subrc EQ 0 ).
    c_r = 'X'.
    PERFORM z_saida_mercadoria USING p_zsdt0001 sl_vbpok vg_sysubrc vl_mat_doc_s vl_matdocumentyear_s c_r p_data_rem.

    IF NOT ( vl_mat_doc_s IS INITIAL ).

*      CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
*        EXPORTING
*          vbkok_wa                 = sl_vbkok_wa
*          synchron                 = 'X'
*          if_error_messages_send_1 = 'X'
*        TABLES
*          vbpok_tab                = tl_vbpok
*          prot                     = tl_prot.

      IF ( tl_prot[] IS INITIAL AND wa_return IS INITIAL ). "AND ( sy-subrc IS INITIAL ).
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*        CLEAR: ZSDT0023.
*        ZSDT0023-VBELN         = SL_VBKOK_WA-VBELN.
*        ZSDT0023-VBELV         = S_VBAP-VBELN.
*        ZSDT0023-CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
*        ZSDT0023-MBLNR_S       = VL_MAT_DOC_S.
*        ZSDT0023-MJAHR_S       = VL_MATDOCUMENTYEAR_S.
*        ZSDT0023-MBLNR_E       = VL_MAT_DOC_E.
*        ZSDT0023-MJAHR_E       = VL_MATDOCUMENTYEAR_E.
*        ZSDT0023-WERKS_V       = WA_DEPARA-WERKS_V.
*        IF S_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL. "ALRS 04.05.2015
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_T.
*        ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_F.
*        ELSE.
*          ZSDT0023-LGORT_V       = WA_DEPARA-LGORT.
*        ENDIF.
*        ZSDT0023-DT_SAIDA      = SY-DATUM.
*        ZSDT0023-HS_SAIDA      = SY-UZEIT.
*        INSERT ZSDT0023.
*        COMMIT WORK.
      ELSE.
*        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*        vg_erro_pic = 'X'.
      ENDIF.


    ENDIF.

  ELSE.

    IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).
      PERFORM z_saida_mercadoria USING p_zsdt0001 sl_vbpok vg_sysubrc vl_mat_doc_s vl_matdocumentyear_s c_r p_data_rem .
    ENDIF.

    IF vg_sysubrc IS INITIAL.

      IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).
        PERFORM gera_material_entrada USING p_delivery vl_mat_doc_s vl_matdocumentyear_s vl_mat_doc_e vl_matdocumentyear_e vg_erro_mat_e p_data_rem p_zsdt0001 sl_vbpok..
      ENDIF.

*      IF vg_erro_mat_e IS INITIAL.
*
*        CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
*          EXPORTING
*            vbkok_wa                 = sl_vbkok_wa
*            synchron                 = 'X'
*            if_error_messages_send_1 = 'X'
*          TABLES
*            vbpok_tab                = tl_vbpok
*            prot                     = tl_prot.
*
*        IF ( tl_prot[] IS INITIAL AND wa_return IS INITIAL ). "AND ( sy-subrc IS INITIAL ).
*          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*            EXPORTING
*              wait = 'X'.
*        ELSE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*          vg_erro_pic = 'X'.
*        ENDIF.
*
*      ENDIF.
*
    ELSE.
      vg_erro_mat_s = 'X'.
    ENDIF.

*    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 15.08.2016
*    IF ( S_VBAK-AUART EQ 'ZRDC' ) OR ( S_VBAK-AUART EQ 'ZRFL' ) OR ( S_VBAK-AUART EQ 'ZIND' ).
*      CLEAR: ZSDT0023.
*      ZSDT0023-VBELN         = SL_VBKOK_WA-VBELN.
*      ZSDT0023-VBELV         = S_VBAP-VBELN.
*      ZSDT0023-CH_REFERENCIA = P_ZSDT0001-CH_REFERENCIA.
*      ZSDT0023-MBLNR_S       = VL_MAT_DOC_S.
*      ZSDT0023-MJAHR_S       = VL_MATDOCUMENTYEAR_S.
*      ZSDT0023-MBLNR_E       = VL_MAT_DOC_E.
*      ZSDT0023-MJAHR_E       = VL_MATDOCUMENTYEAR_E.
*      ZSDT0023-WERKS_V       = WA_DEPARA-WERKS_V.
*      IF S_VBAK-KVGR3 = 'C' AND WA_DEPARA-LGORT_T IS NOT INITIAL. "ALRS 04.05.2015
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_T.
*      ELSEIF S_VBAK-KVGR3 = 'F' AND WA_DEPARA-LGORT_F IS NOT INITIAL. "ALRS 26.01.2018
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT_F.
*      ELSE.
*        ZSDT0023-LGORT_V       = WA_DEPARA-LGORT.
*      ENDIF.
*      ZSDT0023-DT_SAIDA      = SY-DATUM.
*      ZSDT0023-HS_SAIDA      = SY-UZEIT.
*      INSERT ZSDT0023.
*      COMMIT WORK.
*    ENDIF.

*    IF ( vg_erro_pic IS INITIAL ) AND ( vg_erro_mat_s IS INITIAL ) AND ( vg_erro_mat_e IS INITIAL ).
*   Atualiza ZSDT0001
*      UPDATE zsdt0001
*         SET status  = 'X'
*             doc_rem = p_delivery
*             doc_material = vl_mat_doc_r
*             ano_material = vl_matdocumentyear_r
*       WHERE ch_referencia EQ p_zsdt0001-ch_referencia.

*      IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZLES0115' AND sy-tcode NE 'ZLES0136' AND  sy-tcode NE 'ZMM0127' AND sy-batch NE abap_true.
*        UPDATE zsdt0001
*                 SET st_proc = '99'
*               WHERE ch_referencia EQ p_zsdt0001-ch_referencia.
*      ENDIF.

*      CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
*        EXPORTING
*          cd_referencia = p_zsdt0001-ch_referencia
*          tp_bloqueio   = 'X'.
*
*    ENDIF.

*    IF ( NOT vg_erro_pic IS INITIAL ) OR ( NOT vg_erro_mat_s IS INITIAL ) OR ( NOT vg_erro_mat_e IS INITIAL ).
*      IF vl_mat_doc_e IS INITIAL.
*        SELECT SINGLE mblnr,mjahr
*          FROM mkpf
*          INTO ( @vl_mat_doc_e, @vl_matdocumentyear_e )
*           WHERE bktxt  = @p_delivery
*           AND   tcode2 = 'MBSU'.
*        vg_erro_pic = 'X'.
*        REFRESH tl_return.
*        wa_return-type    = 'E'.
*        wa_return-message = |Doc.entrada Recuperado { p_delivery }, { vl_mat_doc_e }  |.
*        APPEND wa_return TO tl_return.
*        PERFORM z_monta_erro TABLES tl_return
*                           USING p_zsdt0001.
*        REFRESH tl_return.
*      ENDIF.
*      IF vl_mat_doc_s IS INITIAL.
*        SELECT SINGLE mblnr,mjahr
*          FROM mkpf
*          INTO ( @vl_mat_doc_s, @vl_matdocumentyear_s )
*           WHERE bktxt  = @p_delivery
*           AND   tcode2 = 'MB1B'.
*        REFRESH tl_return.
*        wa_return-type    = 'E'.
*        wa_return-message = |Doc.saida Recuperado { p_delivery }, { vl_mat_doc_s } |.
*        APPEND wa_return TO tl_return.
*        PERFORM z_monta_erro TABLES tl_return
*                           USING p_zsdt0001.
*        REFRESH tl_return.
*        vg_erro_pic = 'X'.
*      ENDIF.
*      IF ( vg_erro_pic IS NOT INITIAL ) OR ( NOT vg_erro_mat_e IS INITIAL ).
*
*        IF vg_erro_pic IS NOT INITIAL AND vl_mat_doc_e IS NOT INITIAL.
*          "Estorno de material Entrada.
*          PERFORM cancela_doc_material USING p_delivery  vl_mat_doc_e vl_matdocumentyear_e  'E'.
*        ENDIF.
*
*        "Estorno de material Saída.
*        PERFORM cancela_doc_material USING p_delivery vl_mat_doc_s vl_matdocumentyear_s 'S'.
*
*        PERFORM f_estorno_res  CHANGING p_zsdt0001.
*      ENDIF.
*
** BBKO/Vagner Santos - Início da alteração - 02.10.2010
** Salvar a mensagem de erro
*      LOOP AT tl_prot INTO sl_prot.
*        MESSAGE ID sl_prot-msgid
*          TYPE sl_prot-msgty
*        NUMBER sl_prot-msgno
*          WITH sl_prot-msgv1
*               sl_prot-msgv2
*               sl_prot-msgv3
*               sl_prot-msgv4
*          INTO sl_return-message.
*        sl_return-type = sl_prot-msgty.
*        APPEND sl_return TO tl_return.
*        CLEAR: sl_prot  ,
*               sl_return.
*      ENDLOOP.
**   Retorna Erro
*      PERFORM z_monta_erro TABLES tl_return
*                            USING p_zsdt0001.
*
**   Deleta Delivery Criado
*      DATA: sl_hdata    TYPE bapiobdlvhdrchg,
*            sl_hcont    TYPE bapiobdlvhdrctrlchg,
*            vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
*            tl_bapiret2 TYPE bapiret2_t.
*
*      sl_hdata-deliv_numb = p_delivery.
*      sl_hcont-deliv_numb = p_delivery.
*      sl_hcont-dlv_del    = 'X'.
*      vl_delivery         = p_delivery.
*
**      CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
**        EXPORTING
**          header_data    = sl_hdata
**          header_control = sl_hcont
**          delivery       = vl_delivery
**        TABLES
**          return         = tl_bapiret2.
**
**      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
**        EXPORTING
**          wait = 'X'.
**
*      REFRESH tl_return.
*      APPEND LINES OF tl_bapiret2 TO tl_return.
**   Retorna Erro
*      PERFORM z_monta_erro TABLES tl_return
*                            USING p_zsdt0001.
*
*    ENDIF.
*
*  ENDIF.

*  IF ( c_r EQ 'X' ).
*    " Atualiza ZSDT0001
*    UPDATE zsdt0001
*       SET status  = 'X'
*           doc_rem = p_delivery
*           doc_material = vl_mat_doc_r
*           ano_material = vl_matdocumentyear_r
*     WHERE ch_referencia EQ p_zsdt0001-ch_referencia.
*
*    IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZLES0115' AND sy-tcode NE 'ZMM0127' AND sy-batch NE abap_true.
*      UPDATE zsdt0001
*               SET st_proc = '99'
*             WHERE ch_referencia EQ p_zsdt0001-ch_referencia.
*    ENDIF.
*
*    CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
*      EXPORTING
*        cd_referencia = p_zsdt0001-ch_referencia
*        tp_bloqueio   = 'X'.
  ENDIF.

  CLEAR: c_r.
ENDFORM.                    " Z_PICKING
*&---------------------------------------------------------------------*
*&      Form  Z_SAIDA_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_DELIVERY  text
*      -->P_SL_ZSDT0001  text
*----------------------------------------------------------------------*
FORM z_saida_mercadoria  USING    p_zsdt0001  TYPE zsdt0001
                                  sl_vbkok_wa TYPE vbpok
                                  lc_subrc    TYPE sy-subrc
                                  vl_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc
                                  vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year
                                  c_r                TYPE c
                                  p_data_rem         TYPE ledat.

  DATA: msg_text TYPE string,
        w_msn    TYPE type_msn,
        msg_id   TYPE t100-arbgb,
        msg_no   TYPE t100-msgnr,
        msg_var1 TYPE balm-msgv1,
        w_return TYPE bapiret2.


  DATA: wa_kna1 TYPE kna1,
        wa_lfa1 TYPE lfa1.



  CLEAR: wa_goodsmvt_header, t_goodsmvt_item.

  lc_subrc = 0.
  wa_goodsmvt_header-pstng_date = p_data_rem.
  wa_goodsmvt_header-doc_date   = p_data_rem.
  wa_goodsmvt_header-header_txt = sl_vbkok_wa-vbeln.
  wa_code-gm_code               = c_04.
*--> 19.06.2023 - Migration S4 – MIGNOW - Start
  "  wa_goodsmvt_item-material     = s_vbap-matnr.
  DATA(v_len2) = strlen( s_vbap-matnr ).
  IF v_len2 > 18.
    wa_goodsmvt_item-material_long = s_vbap-matnr .
  ELSE.
    wa_goodsmvt_item-material = s_vbap-matnr .
  ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
  wa_goodsmvt_item-plant        = s_vbap-vstel.
  wa_goodsmvt_item-stge_loc	    = s_vbap-lgort.
  wa_goodsmvt_item-batch        = s_vbap-charg.
  wa_goodsmvt_item-gr_rcpt      = s_vbap-vbeln.
  wa_goodsmvt_item-item_text    = s_vbap-vbeln .

  CASE s_vbak-auart.
    WHEN 'ZIND'.
      wa_goodsmvt_item-move_type    = 'I50'.
    WHEN OTHERS.
      wa_goodsmvt_item-move_type    = c_f50.
  ENDCASE.

  wa_goodsmvt_item-entry_qnt    = sl_vbkok_wa-pikmg. "SL_VBKOK_WA-BRGEW.


  IF ( c_r EQ 'X' ).

    SELECT SINGLE * FROM kna1 INTO wa_kna1 WHERE kunnr EQ s_vbak-kunnr.

    IF ( sy-subrc EQ 0 ).

      SELECT SINGLE * FROM lfa1 INTO wa_lfa1 WHERE stcd1 EQ wa_kna1-stcd1.

      IF ( sy-subrc EQ 0 ).
        wa_goodsmvt_item-vendor    = wa_lfa1-lifnr.

        CLEAR: wa_goodsmvt_item-move_type.
        CASE p_zsdt0001-tp_movimento.
          WHEN: 'E'.
            wa_goodsmvt_item-move_type = '542'.
          WHEN: 'S'.
            wa_goodsmvt_item-move_type = '541'.
        ENDCASE.

      ELSE.
        MESSAGE i836 WITH 'Fornecedor não cadastrado'.
      ENDIF.

    ENDIF.

  ENDIF.

  APPEND wa_goodsmvt_item TO t_goodsmvt_item.

*Exeuta a BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = wa_goodsmvt_header
      goodsmvt_code    = wa_code
    IMPORTING
      materialdocument = vl_mat_doc
      matdocumentyear  = vl_matdocumentyear
    TABLES
      goodsmvt_item    = t_goodsmvt_item
      return           = t_return.

  READ TABLE t_return INTO wa_return WITH KEY type = c_e.
  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    lc_subrc = 4.

    LOOP AT t_return INTO w_return.
      WRITE w_return-number TO msg_no.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = w_return-id
          msg_no                 = msg_no
          msg_var1               = w_return-message_v1
          msg_var2               = w_return-message_v2
          msg_var3               = w_return-message_v3
          msg_var4               = w_return-message_v4
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
      UPDATE zsdt0023_temp SET mensagem = @msg_text
                         WHERE vbeln    = @sl_vbkok_wa-vbeln
                           AND seq      = @l_timestamp.
*JJJJJJJJJJJJJJJJJJJJJJJJJ

      w_msn-messagem = msg_text.
      w_msn-tp_msn   = w_return-type.
      APPEND w_msn TO t_msn.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    msg_id   = 'M7'.
    msg_no   = '060'.
    msg_var1 = vl_mat_doc.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = msg_id
        msg_no                 = msg_no
        msg_var1               = msg_var1
      IMPORTING
        msg_text               = msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    w_msn-messagem = msg_text.
    w_msn-tp_msn   = 'S'.
    APPEND w_msn TO t_msn.
    "
    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 2 (saida )
    UPDATE zsdt0023 SET mblnr_s  = @vl_mat_doc,
                        mjahr_s  = @vl_matdocumentyear
    WHERE vbeln = @sl_vbkok_wa-vbeln.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
    UPDATE zsdt0023_temp SET mblnr_s  = @vl_mat_doc,
                             mjahr_s  = @vl_matdocumentyear
                       WHERE vbeln    = @sl_vbkok_wa-vbeln
                         AND seq      = @l_timestamp.
*JJJJJJJJJJJJJJJJJJJJJJJJJ

    COMMIT WORK.
  ENDIF.

ENDFORM.                    " Z_SAIDA_MERCADORIA



*&---------------------------------------------------------------------*
*&      Form  Z_CAP_VEICULO                                            *
*&---------------------------------------------------------------------*
*                          Capacidade Veículo                          *
*----------------------------------------------------------------------*
FORM z_cap_veiculo USING p_zsdt0001 TYPE zsdt0001.

  DATA sl_msn TYPE type_msn.

  sl_msn-nr_romaneio = p_zsdt0001-nr_romaneio.
  sl_msn-tp_msn      = 'E'.
  sl_msn-messagem    = TEXT-018.
  APPEND sl_msn TO t_msn.

ENDFORM.                    " Z_CAP_VEICULO

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_TEXTO                                            *
*&---------------------------------------------------------------------*
*                               Salva Textos                           *
*----------------------------------------------------------------------*
FORM z_salva_texto USING p_delivery TYPE vbeln_vl
                         p_zsdt0001 TYPE zsdt0001.

  DATA: sl_header TYPE thead,
        sl_lines  TYPE tline,
        vl_text   TYPE char256,
        tl_lines  TYPE TABLE OF tline.

  REFRESH tl_lines.

  sl_header-tdobject = 'VBBK'.
  sl_header-tdname   =  p_delivery.
  sl_header-tdid     = '0001'.
  sl_header-tdspras  = 'P'.

  CONCATENATE 'Pesagem OPUS ChRef:'
              p_zsdt0001-ch_referencia
         INTO vl_text SEPARATED BY space.
  sl_lines-tdformat  = '*'.
  sl_lines-tdline    = vl_text.
  APPEND sl_lines TO tl_lines.
  CLEAR: sl_lines,
         vl_text.
  CONCATENATE 'Número do Romaneio:'
              p_zsdt0001-nr_romaneio
         INTO vl_text SEPARATED BY space.
  sl_lines-tdformat  = '*'.
  sl_lines-tdline    = vl_text.
  APPEND sl_lines TO tl_lines.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = sl_header
      savemode_direct = 'X'
    TABLES
      lines           = tl_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

ENDFORM.                    " Z_SALVA_TEXTO

*&---------------------------------------------------------------------*
*&      Form  Z_REG_SM                                                 *
*&---------------------------------------------------------------------*
*                            Registrar SM                              *
*----------------------------------------------------------------------*
FORM z_reg_sm USING p_vdelivery TYPE vbeln_vl.

  DATA: vl_mode TYPE c VALUE 'N',
        tl_itab TYPE TABLE OF bdcmsgcoll.

  REFRESH t_bdc.

* Insere BDC
  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '4004'     ,
                              ' ' 'BDC_OKCODE' '=WABU_T'  ,
                              ' ' 'LIKP-VBELN' p_vdelivery.

  CALL FUNCTION 'DEQUEUE_ALL'
    EXPORTING
      _synchron = 'X'.

  WAIT UP TO 2 SECONDS.

  CALL TRANSACTION 'VL02N'
     USING t_bdc
     MODE vl_mode
     MESSAGES INTO tl_itab.

ENDFORM.                    " Z_REG_SM

FORM z_ins_lote USING p_vdelivery TYPE vbeln_vl
                      p_charg     TYPE vbpok-charg.

  DATA: vl_mode TYPE c VALUE 'N',
        tl_itab TYPE TABLE OF bdcmsgcoll.

  REFRESH t_bdc.

* Insere BDC
  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '4004'     ,
                              ' ' 'BDC_OKCODE' '/00'  ,
                              ' ' 'LIKP-VBELN' p_vdelivery.

  PERFORM z_insere_bdc USING: 'X' 'SAPMV50A'   '1000'     ,
                              ' ' 'BDC_OKCODE' '=SICH_T'  ,
                              ' ' 'LIPS-CHARG(01)' p_charg.
*  CALL FUNCTION 'DEQUEUE_ALL'
*    EXPORTING
*      _synchron = 'X'.

*  WAIT UP TO 1 SECONDS.

*  CALL TRANSACTION 'VL02N'
*     USING t_bdc
*     MODE vl_mode
*     MESSAGES INTO tl_itab.
*  COMMIT WORK.
*  WAIT UP TO 2 SECONDS.

ENDFORM.                    " Z_REG_SM

*&---------------------------------------------------------------------*
*&      Form  Z_INSERE_BDC                                             *
*&---------------------------------------------------------------------*
*                             Insere BDC                               *
*----------------------------------------------------------------------*
FORM z_insere_bdc USING p_dynbegin TYPE any
                        p_field    TYPE any
                        p_value    TYPE any.

  DATA sl_bdc TYPE bdcdata.

  CLEAR sl_bdc.

  IF p_dynbegin EQ 'X'.
    sl_bdc-dynbegin = 'X'.
    sl_bdc-program  = p_field.
    sl_bdc-dynpro   = p_value.
  ELSE.
    sl_bdc-fnam = p_field.
    sl_bdc-fval = p_value.
  ENDIF.

  APPEND sl_bdc TO t_bdc.

ENDFORM.                    " Z_INSERE_BDC
* BBKO/Vagner Santos - Início da alteração - 02.10.2010
*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_AGENTE_FRETE
*&---------------------------------------------------------------------*
* Consistir o código do agente de frete informado.
*----------------------------------------------------------------------*
FORM z_verifica_agente_frete RAISING zcx_error. "*-#133089-21.02.2024-JT-inicio
  .

  IF NOT p_lifnr IS INITIAL.
    CHECK p_lifnr-low IS NOT INITIAL. "//Quando vazio, é uma fatura agrupada;
    SELECT SINGLE lifnr INTO lfa1-lifnr
                        FROM lfa1
    WHERE lifnr IN p_lifnr.

    IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
      CASE vg_faturamento_autom.
        WHEN abap_off.
          MESSAGE i836 WITH TEXT-022.
          LEAVE LIST-PROCESSING.
        WHEN abap_true.
          MESSAGE i836 WITH TEXT-022 INTO DATA(l_mesg).
          lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
          lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
      ENDCASE.
*-#133089-21.02.2024-JT-fim
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_AGENTE_FRETE
*&---------------------------------------------------------------------*
*&      Form  Z_INCLUIR_AGENTE_FRETE
*&---------------------------------------------------------------------*
* Executar a bapi de modificação da remessa
*----------------------------------------------------------------------*
FORM z_incluir_agente_frete  USING    p_delivery TYPE vbeln_vl
                                      p_zsdt0001 TYPE zsdt0001.

  DATA: tl_bapiret2 TYPE bapiret2_t,
        tl_return   TYPE bapiret2_t,
        sl_bapiret2 TYPE bapiret2,
        sl_return   TYPE bapiret2.

  DATA: wa_header_data    TYPE bapiobdlvhdrchg,
        wa_header_control TYPE bapiobdlvhdrctrlchg,
        header_partner    TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE.

  READ TABLE p_lifnr.
  IF NOT p_lifnr-low IS INITIAL.

    wa_header_data-deliv_numb    = p_delivery.
    wa_header_control-deliv_numb = 'X'.

* Incluir o parceiro SP (Agente de frete) na remessa
    IF ( s_vbak-auart EQ 'ZRDC' ) OR ( s_vbak-auart EQ 'ZRFL' ) OR ( s_vbak-auart EQ 'ZIND' ).
      header_partner-upd_mode_partn = 'U'.
    ELSE.
      header_partner-upd_mode_partn = 'I'.
    ENDIF.

    header_partner-deliv_numb     = p_delivery.
    header_partner-itm_number     = '000010'.
    header_partner-partn_role     = 'SP'.
    header_partner-partner_no     = p_lifnr-low.
    APPEND header_partner.

*    CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*      EXPORTING
*        header_data    = wa_header_data
*        header_control = wa_header_control
*        delivery       = p_delivery
*      TABLES
*        header_partner = header_partner
*        return         = tl_bapiret2.
*
*    IF tl_bapiret2[] IS INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = 'X'.
*    ELSE.
*      LOOP AT tl_bapiret2 INTO sl_bapiret2.
*        MESSAGE ID sl_bapiret2-id
*          TYPE sl_bapiret2-type
*        NUMBER sl_bapiret2-number
*          WITH sl_bapiret2-message_v1
*               sl_bapiret2-message_v2
*               sl_bapiret2-message_v3
*               sl_bapiret2-message_v4
*          INTO sl_return-message.
*        sl_return-type = sl_bapiret2-type.
*        APPEND sl_return TO tl_return.
*        CLEAR: sl_bapiret2,
*               sl_return.
*      ENDLOOP.
*
**     Retorna Erro
*      PERFORM z_monta_erro TABLES tl_return
*                            USING p_zsdt0001.
*    ENDIF.
  ENDIF.

ENDFORM.                    " Z_INCLUIR_AGENTE_FRETE
* BBKO/Vagner Santos - Fim da alteração - 02.10.2010

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_PARC                                          *
*&---------------------------------------------------------------------*
*                            Verifica Parceiros                        *
*----------------------------------------------------------------------*
FORM z_verifica_parc USING p_in    TYPE vbak-auart
                  CHANGING p_auart TYPE zzauart.

  DATA: tl_vbpa    TYPE TABLE OF vbpa,
        sl_vbpa_lr TYPE vbpa,
        sl_vbpa_z1 TYPE vbpa,
        vl_cnpj_lr TYPE kna1-stcd1,
        vl_cnpj_z1 TYPE lfa1-stcd1.

  CLEAR p_auart.

  SELECT *
    FROM vbpa
    INTO TABLE tl_vbpa
  WHERE  vbeln EQ s_vbak-vbeln.

  DELETE tl_vbpa WHERE parvw NE 'LR'
                   AND parvw NE 'Z1'.
  SORT tl_vbpa BY parvw ASCENDING.

  READ TABLE tl_vbpa: INTO sl_vbpa_lr WITH KEY parvw = 'LR',
                      INTO sl_vbpa_z1 WITH KEY parvw = 'Z1'.

  SELECT SINGLE stcd1
    FROM kna1
    INTO vl_cnpj_lr
  WHERE  kunnr EQ sl_vbpa_lr-kunnr.

  SELECT SINGLE stcd1
    FROM lfa1
    INTO vl_cnpj_z1
  WHERE  lifnr EQ sl_vbpa_z1-lifnr.

  IF vl_cnpj_lr EQ vl_cnpj_z1.
    CASE p_in.
      WHEN 'ZRFL'.
        p_auart = 'ZRFL1'.
      WHEN 'ZRDC'.
        p_auart = 'ZRDC1'.
    ENDCASE.
  ELSE.
    CASE p_in.
      WHEN 'ZRFL'.
        p_auart = 'ZRFL2'.
      WHEN 'ZRDC'.
        p_auart = 'ZRDC2'.
    ENDCASE.
  ENDIF.

ENDFORM.                    " Z_VERIFICA_PARC

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVARVC                                       *
*&---------------------------------------------------------------------*
*                             Verifica Usuário                         *
*----------------------------------------------------------------------*
FORM z_seleciona_tvarvc.

  DATA: wa_setleaf LIKE setleaf.

  CLEAR v_user.

*  SELECT SINGLE LOW
*    FROM TVARVC
*    INTO V_USER
*  WHERE  NAME EQ 'REM_DATA_RETROATIVA'
*    AND  LOW  EQ SY-UNAME.

  SELECT SINGLE *
    FROM setleaf
    INTO wa_setleaf
   WHERE setname = 'VF01_USUARIO'
  AND valfrom = sy-uname.

  IF sy-subrc  IS INITIAL.
    v_user = sy-uname.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_TVARVC
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_MAT_DOC  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM z_monta_sucesso_material  USING    p_vl_mat_doc
                                        p_space.

  DATA: sl_msn_mat  TYPE type_msn,
        sl_text_mat TYPE char50.
  sl_msn_mat-tp_msn      = 'S'.

  IF NOT p_vl_mat_doc IS INITIAL.
    sl_text_mat = 'Documento de Material Criado: '.
  ENDIF.

  CONCATENATE sl_text_mat
              p_vl_mat_doc
         INTO sl_msn_mat-messagem SEPARATED BY space.
  APPEND sl_msn_mat TO t_msn.


ENDFORM.                    " Z_MONTA_MATERIAL
*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO_DEPARA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_LIFNR  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM z_monta_erro_depara  USING    p_p_lifnr
                                   p_space.

  DATA: sl_msn_depara  TYPE type_msn,
        sl_text_depara TYPE char100,
        sl_text_final  TYPE char100.

  sl_msn_depara-tp_msn      = 'S'.

  IF NOT p_p_lifnr IS INITIAL.
    sl_text_depara = 'Centro Virtual não encontrado para o centro: '.
    sl_text_final = ' por favor ligar para a Área de Execução.'.
  ENDIF.

  CONCATENATE sl_text_depara
              p_p_lifnr
              sl_text_final
         INTO sl_msn_depara-messagem SEPARATED BY space.
  APPEND sl_msn_depara TO t_msn.

ENDFORM.                    " Z_MONTA_ERRO_DEPARA
*&---------------------------------------------------------------------*
*&      Form  Z_ERRO_ESTORNO_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_MAT_DOC  text
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM z_erro_estorno_material  USING    p_vl_mat_doc
                                       p_space.

  DATA: sl_msn_estorno  TYPE type_msn,
        sl_text_estorno TYPE char100.

  sl_msn_estorno-tp_msn      = 'E'.

  IF NOT p_vl_mat_doc IS INITIAL.
    sl_text_estorno = 'Ocorreu um erro no estorno no documento de material. '.
  ENDIF.

  CONCATENATE sl_text_estorno
               p_vl_mat_doc
         INTO sl_msn_estorno-messagem SEPARATED BY space.
  APPEND sl_msn_estorno TO t_msn.


ENDFORM.                    " Z_ERRO_ESTORNO_MATERIAL

*&---------------------------------------------------------------------*
*&      Form  GERA_MATERIAL_ENTRADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_material_entrada USING vg_remessa           TYPE vbeln_vl
                                 vl_mat_doc           TYPE bapi2017_gm_head_ret-mat_doc
                                 vl_matdocumentyear   TYPE bapi2017_gm_head_ret-doc_year
                                 vl_mat_doc_e         TYPE bapi2017_gm_head_ret-mat_doc
                                 vl_matdocumentyear_e TYPE bapi2017_gm_head_ret-doc_year
                                 vg_erro_mat_e        TYPE c
                                 p_data_rem           TYPE ledat
                                 p_zsdt0001           TYPE zsdt0001
                                 sl_vbkok_wa          TYPE vbpok.


  "-- Projeto S4 Hana - Conversao para bapi - Fim
  DATA: msg_text TYPE string,
        w_msn    TYPE type_msn,
        msg_id   TYPE t100-arbgb,
        msg_no   TYPE t100-msgnr,
        msg_var1 TYPE balm-msgv1,
        w_return TYPE bapiret2.


  DATA: wa_kna1 TYPE kna1,
        wa_lfa1 TYPE lfa1.

  CLEAR: wa_goodsmvt_header, t_goodsmvt_item, wa_goodsmvt_item, vg_erro_mat_e.

  wa_goodsmvt_header-pstng_date = p_data_rem.
  wa_goodsmvt_header-doc_date   = p_data_rem.
  wa_goodsmvt_header-header_txt = vg_remessa.
  wa_code-gm_code               = c_05.
  DATA(v_len2) = strlen( s_vbap-matnr ).
  IF v_len2 > 18.
    wa_goodsmvt_item-material_long = s_vbap-matnr .
  ELSE.
    wa_goodsmvt_item-material = s_vbap-matnr .
  ENDIF.

  wa_goodsmvt_item-plant        = wa_depara-werks_v.
  "wa_goodsmvt_item-move_plant   = s_vbap-vstel.

  IF wa_vbak-kvgr3 = 'C' AND wa_depara-lgort_t IS NOT INITIAL.
    wa_goodsmvt_item-stge_loc	    =  wa_depara-lgort_t.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort_t.
  ELSEIF s_vbak-kvgr3 = 'F' AND wa_depara-lgort_f IS NOT INITIAL.
    wa_goodsmvt_item-stge_loc	    =  wa_depara-lgort_f.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort_f.
  ELSE.
    wa_goodsmvt_item-stge_loc	    =  wa_depara-lgort.
    "wa_goodsmvt_item-move_stloc   =  wa_depara-lgort.
  ENDIF.

  wa_goodsmvt_item-batch        = s_vbap-charg.

*  wa_goodsmvt_item-move_type    = 'F52'.

  CASE s_vbak-auart.
    WHEN 'ZIND'.
      wa_goodsmvt_item-move_type    = 'I52'.
    WHEN OTHERS.
      wa_goodsmvt_item-move_type    = 'F52'.
  ENDCASE.
  wa_goodsmvt_item-entry_qnt    = sl_vbkok_wa-pikmg. "SL_VBKOK_WA-BRGEW.

  APPEND wa_goodsmvt_item TO t_goodsmvt_item.

*Exeuta a BAPI
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = wa_goodsmvt_header
      goodsmvt_code    = wa_code
    IMPORTING
      materialdocument = vl_mat_doc
      matdocumentyear  = vl_matdocumentyear
    TABLES
      goodsmvt_item    = t_goodsmvt_item
      return           = t_return.

  READ TABLE t_return INTO wa_return WITH KEY type = c_e.
  IF sy-subrc IS INITIAL.
    vg_erro_mat_e = 'X'.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    LOOP AT t_return INTO w_return.
      WRITE w_return-number TO msg_no.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = w_return-id
          msg_no                 = msg_no
          msg_var1               = w_return-message_v1
          msg_var2               = w_return-message_v2
          msg_var3               = w_return-message_v3
          msg_var4               = w_return-message_v4
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
      UPDATE zsdt0023_temp SET mensagem = @msg_text
                         WHERE vbeln    = @vg_remessa
                           AND seq      = @l_timestamp.
*JJJJJJJJJJJJJJJJJJJJJJJJJ

      w_msn-messagem = msg_text.
      w_msn-tp_msn   = w_return-type.
      APPEND w_msn TO t_msn.
    ENDLOOP.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    msg_id   = 'M7'.
    msg_no   = '060'.
    msg_var1 = vl_mat_doc.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = msg_id
        msg_no                 = msg_no
        msg_var1               = msg_var1
      IMPORTING
        msg_text               = msg_text
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    w_msn-messagem = msg_text.
    w_msn-tp_msn   = 'S'.
    APPEND w_msn TO t_msn.

    UPDATE zsdt0023 SET mblnr_e  = @vl_mat_doc,
                        mjahr_e  = @vl_matdocumentyear
    WHERE vbeln = @vg_remessa.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
    UPDATE zsdt0023_temp SET mblnr_e  = @vl_mat_doc,
                             mjahr_e  = @vl_matdocumentyear
                       WHERE vbeln    = @vg_remessa
                         AND seq      = @l_timestamp.
*JJJJJJJJJJJJJJJJJJJJJJJJJ

    COMMIT WORK.
  ENDIF.

  EXIT.

  "-- Projeto S4 Hana - Conversao para bapi - Fim

  DATA: wa_data   TYPE c LENGTH 10,
        vg_texto  TYPE string,
*        wa_likp   TYPE likp,
        w_messtab TYPE bdcmsgcoll.
  "w_msn     TYPE type_msn, "-- Projeto S4 Hana - Conversao para bapi - Fim
  "msg_text  TYPE string.   "-- Projeto S4 Hana - Conversao para bapi - Fim

  CLEAR: vg_erro_mat_e.

  WRITE p_data_rem TO wa_data.

  CONCATENATE 'Entr Form Lote:' vg_remessa INTO vg_texto SEPARATED BY space.

  CLEAR: t_bdc[], t_bdc, t_messtab[], t_messtab.
  "ALRS
  SELECT SINGLE *
         FROM vbak
         INTO wa_vbak
  WHERE vbeln = p_zsdt0001-vbeln.

  IF wa_vbak-kvgr3 = 'C' AND wa_depara-lgort_t IS NOT INITIAL.
    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'      '0460',
                                ' ' 'BDC_OKCODE'    '/00',
                                ' ' 'MKPF-BUDAT'   wa_data,
                                ' ' 'RM07M-MBLNR'  vl_mat_doc,
                                ' ' 'RM07M-MJAHR'  vl_matdocumentyear,
                                ' ' 'RM07M-LGORT'  wa_depara-lgort_t,
                                ' ' 'RM07M-WVERS2'  'X'.
  ELSEIF s_vbak-kvgr3 = 'F' AND wa_depara-lgort_f IS NOT INITIAL. "ALRS 26.01.2018
    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'      '0460',
                                    ' ' 'BDC_OKCODE'    '/00',
                                    ' ' 'MKPF-BUDAT'   wa_data,
                                    ' ' 'RM07M-MBLNR'  vl_mat_doc,
                                    ' ' 'RM07M-MJAHR'  vl_matdocumentyear,
                                    ' ' 'RM07M-LGORT'  wa_depara-lgort_f,
                                    ' ' 'RM07M-WVERS2'  'X'.
  ELSE.
    PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'      '0460',
                                    ' ' 'BDC_OKCODE'    '/00',
                                    ' ' 'MKPF-BUDAT'   wa_data,
                                    ' ' 'RM07M-MBLNR'  vl_mat_doc,
                                    ' ' 'RM07M-MJAHR'  vl_matdocumentyear,
                                    ' ' 'RM07M-LGORT'  wa_depara-lgort,
                                    ' ' 'RM07M-WVERS2'  'X'.
  ENDIF.

  PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'    '0421',
                              ' ' 'BDC_OKCODE'  '=SP',
                              ' ' 'BDC_SUBSCR'  'SAPMM07M',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB',
                              ' ' 'DKACB-FMORE' 'X'.

  PERFORM z_insere_bdc USING: 'X' 'SAPLKACB'    '0002',
                              ' ' 'BDC_OKCODE'  '=ENTE',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB'.

  PERFORM z_insere_bdc USING: 'X' 'SAPMM07M'    '0410',
                              ' ' 'BDC_OKCODE'  'BU',
                              ' ' 'BDC_SUBSCR'  'MSEG-SGTXT',
                              ' ' 'MSEG-SGTXT'  vg_texto,
                              ' ' 'BDC_SUBSCR'  'SAPMM07M',
                              ' ' 'BDC_SUBSCR'  'SAPMM07M'.

  PERFORM z_insere_bdc USING: 'X' 'SAPLKACB'    '0002',
                              ' ' 'BDC_OKCODE'  '=ENTE',
                              ' ' 'BDC_SUBSCR'  'SAPLKACB'.

  CALL TRANSACTION 'MBSU' USING t_bdc MODE 'N' UPDATE 'S' MESSAGES INTO t_messtab.

  READ TABLE t_messtab INTO w_messtab WITH KEY msgtyp = 'S' msgnr = '060'.

  IF NOT sy-subrc IS INITIAL.
    vg_erro_mat_e = 'X'.
    LOOP AT t_messtab INTO w_messtab.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = w_messtab-msgid
          msg_no                 = w_messtab-msgnr
          msg_var1               = w_messtab-msgv1(50)
          msg_var2               = w_messtab-msgv2(50)
          msg_var3               = w_messtab-msgv3(50)
          msg_var4               = w_messtab-msgv4(50)
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      w_msn-messagem = msg_text.
      w_msn-tp_msn   = w_messtab-msgtyp.
      APPEND w_msn TO t_msn.
    ENDLOOP.
  ELSE.

    vl_mat_doc_e         = w_messtab-msgv1(10).
    vl_matdocumentyear_e = vl_matdocumentyear.

    " Gravar sempre na ZSDT0023 Mesmo com erro- ALRS 17.02.2020 parte 3 (entrada )
    UPDATE zsdt0023 SET mblnr_e  = @vl_mat_doc_e,
                        mjahr_e  = @vl_matdocumentyear_e
    WHERE vbeln = @vg_remessa.

*JJJJJJJJJJJJJJJJJJJJJJJJJ
    UPDATE zsdt0023_temp SET mblnr_e  = @vl_mat_doc_e,
                             mjahr_e  = @vl_matdocumentyear_e
                       WHERE vbeln    = @vg_remessa
                         AND seq      = @l_timestamp.
*JJJJJJJJJJJJJJJJJJJJJJJJJ

    COMMIT WORK.

    LOOP AT t_messtab INTO w_messtab.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = w_messtab-msgid
          msg_no                 = w_messtab-msgnr
          msg_var1               = w_messtab-msgv1(50)
          msg_var2               = w_messtab-msgv2(50)
          msg_var3               = w_messtab-msgv3(50)
          msg_var4               = w_messtab-msgv4(50)
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      w_msn-messagem = msg_text.
      w_msn-tp_msn   = w_messtab-msgtyp.
      APPEND w_msn TO t_msn.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " GERA_MATERIAL_ENTRADA

*&---------------------------------------------------------------------*
*&      Form  CANCELA_DOC_MATERIAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM cancela_doc_material  USING vg_remessa           TYPE vbeln_vl
                                 vl_mat_doc           TYPE bapi2017_gm_head_ret-mat_doc
                                 vl_matdocumentyear   TYPE bapi2017_gm_head_ret-doc_year
                                 vg_tipo              TYPE c.

*  DATA: goodsmvt_headret TYPE bapi2017_gm_head_ret,
*        w_msn            TYPE type_msn,
*        msg_text         TYPE string,
*        v_loop           TYPE i,
*        vg_bloqueado     TYPE c LENGTH 1.
*
*  DATA: msg_id   LIKE  t100-arbgb,
*        msg_no   LIKE  t100-msgnr,
*        msg_var1 LIKE  balm-msgv1,
*        msg_var2 LIKE  balm-msgv2,
*        msg_var3 LIKE  balm-msgv3,
*        msg_var4 LIKE  balm-msgv4.
*
*  vg_bloqueado = 'X'.
*  CLEAR v_loop.
*
*  WHILE NOT vg_bloqueado IS INITIAL.
*    ADD 1 TO  v_loop.
*    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*      EXPORTING
*        materialdocument    = vl_mat_doc
*        matdocumentyear     = vl_matdocumentyear
*        goodsmvt_pstng_date = sy-datum
*      IMPORTING
*        goodsmvt_headret    = goodsmvt_headret
*      TABLES
*        return              = t_return.
*
*    READ TABLE t_return WITH KEY id = 'M3' number = 897.
*    IF NOT sy-subrc IS INITIAL.
*      CLEAR: vg_bloqueado.
*      "ALRS
*      IF v_loop GT 1.
*        LOOP AT t_return.
*          MOVE: t_return-id         TO msg_id  ,
*               t_return-number     TO msg_no  ,
*               t_return-message_v1 TO msg_var1,
*               t_return-message_v2 TO msg_var2,
*               t_return-message_v3 TO msg_var3,
*               t_return-message_v4 TO msg_var4.
*
*          CALL FUNCTION 'MESSAGE_PREPARE'
*            EXPORTING
*              msg_id                 = msg_id
*              msg_no                 = msg_no
*              msg_var1               = msg_var1
*              msg_var2               = msg_var2
*              msg_var3               = msg_var3
*              msg_var4               = msg_var4
*            IMPORTING
*              msg_text               = msg_text
*            EXCEPTIONS
*              function_not_completed = 1
*              message_not_found      = 2
*              OTHERS                 = 3.
*
*          w_msn-messagem = msg_text.
*          w_msn-tp_msn   = t_return-type.
*          APPEND w_msn TO t_msn.
*        ENDLOOP.
*      ENDIF.
*    ELSE.
*      MOVE: t_return-id         TO msg_id  ,
*            t_return-number     TO msg_no  ,
*            t_return-message_v1 TO msg_var1,
*            t_return-message_v2 TO msg_var2,
*            t_return-message_v3 TO msg_var3,
*            t_return-message_v4 TO msg_var4.
*
*      CALL FUNCTION 'MESSAGE_PREPARE'
*        EXPORTING
*          msg_id                 = msg_id
*          msg_no                 = msg_no
*          msg_var1               = msg_var1
*          msg_var2               = msg_var2
*          msg_var3               = msg_var3
*          msg_var4               = msg_var4
*        IMPORTING
*          msg_text               = msg_text
*        EXCEPTIONS
*          function_not_completed = 1
*          message_not_found      = 2
*          OTHERS                 = 3.
*
*      w_msn-messagem = msg_text.
*      w_msn-tp_msn   = t_return-type.
*      APPEND w_msn TO t_msn.
*
*      WAIT UP TO 10 SECONDS.
*
*    ENDIF.
*
*  ENDWHILE.
*
*
*  READ TABLE t_return WITH KEY type = 'E'.
*  IF NOT sy-subrc IS INITIAL.
*    CONCATENATE 'Documento' vl_mat_doc 'estornado por documento' goodsmvt_headret-mat_doc 'ano' goodsmvt_headret-doc_year '!' INTO msg_text SEPARATED BY space.
*    w_msn-messagem = msg_text.
*    w_msn-tp_msn   = 'S'.
*    APPEND w_msn TO t_msn.
*
*    IF   vg_tipo  = 'S'.
*      UPDATE zsdt0023 SET es_mblnr_s = goodsmvt_headret-mat_doc
*                          es_mjahr_s = goodsmvt_headret-doc_year
*      WHERE vbeln = vg_remessa.
*    ELSE.
*      UPDATE zsdt0023 SET es_mblnr_e = goodsmvt_headret-mat_doc
*                          es_mjahr_e = goodsmvt_headret-doc_year
*      WHERE vbeln = vg_remessa.
*    ENDIF.
*
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ELSE.
*    "Deu Erro no estorno no documento de material
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    PERFORM: z_erro_estorno_material USING vl_mat_doc space.
*  ENDIF.
*
*
ENDFORM.                    " CANCELA_DOC_MATERIAL

FORM f_estorno_res  CHANGING p_zsdt0001 TYPE zsdt0001.
  DATA: w_romaneio               TYPE zsdt0001,
        wa_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
        wa_doc_year              TYPE bapi2017_gm_head_02-doc_year,
        wa_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        v_budat                  TYPE mkpf-budat,
        w_mseg                   TYPE mseg.

  DATA: wa_goodsmvt_header TYPE bapi2017_gm_head_01,
        t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
        wa_code            TYPE bapi2017_gm_code,
        vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year,
        t_return_vt        LIKE bapiret2 OCCURS 0 WITH HEADER LINE.


  "Estorna entrada de Residuo
  SELECT SINGLE *
    FROM zsdt0001
    INTO w_romaneio
  WHERE ch_referencia = p_zsdt0001-ch_referencia.

  IF w_romaneio-doc_material_e IS NOT INITIAL. "doc. material entrada residuo existe
    REFRESH t_return_vt.
    SELECT SINGLE budat INTO v_budat
      FROM mkpf
      WHERE mblnr = w_romaneio-doc_material_e
    AND   mjahr = w_romaneio-ano_material_e.
    "
    wa_mat_doc      = w_romaneio-doc_material_e.
    wa_doc_year    	= w_romaneio-ano_material_e.
    wa_pstng_date   = v_budat.

*    SELECT SINGLE *
*      INTO W_MSEG
*      FROM MSEG
*      WHERE MBLNR = W_ROMANEIO-DOC_MATERIAL_E
*      AND   MJAHR = W_ROMANEIO-ANO_MATERIAL_E
*    AND   BWART = 'ZX1'. "inverte
*
*    IF SY-SUBRC = 0.
*      CLEAR: T_GOODSMVT_ITEM.
*      WA_GOODSMVT_HEADER-PSTNG_DATE = V_BUDAT.
*      WA_GOODSMVT_HEADER-DOC_DATE   = V_BUDAT.
*      WA_GOODSMVT_HEADER-HEADER_TXT = W_ROMANEIO-VBELN.
*
*      WA_CODE-GM_CODE               = '05'.
*
*      WA_GOODSMVT_ITEM-MATERIAL     = W_MSEG-MATNR.
*      WA_GOODSMVT_ITEM-PLANT        = W_MSEG-WERKS.
*      WA_GOODSMVT_ITEM-STGE_LOC      = W_MSEG-LGORT.
*      WA_GOODSMVT_ITEM-BATCH        = W_MSEG-CHARG.
*
*      WA_GOODSMVT_ITEM-MOVE_TYPE    = 'ZX5'. "ALRS 24/05/2017
*      WA_GOODSMVT_ITEM-ENTRY_QNT    = W_MSEG-MENGE.
*      APPEND WA_GOODSMVT_ITEM TO T_GOODSMVT_ITEM.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*        EXPORTING
*          GOODSMVT_HEADER  = WA_GOODSMVT_HEADER
*          GOODSMVT_CODE    = WA_CODE
*        IMPORTING
*          MATERIALDOCUMENT = VL_MAT_DOC
*          MATDOCUMENTYEAR  = VL_MATDOCUMENTYEAR
*        TABLES
*          GOODSMVT_ITEM    = T_GOODSMVT_ITEM
*          RETURN           = T_RETURN_VT.
*    ELSE.
*    CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
*      EXPORTING
*        materialdocument    = wa_mat_doc
*        matdocumentyear     = wa_doc_year
*        goodsmvt_pstng_date = wa_pstng_date
*      IMPORTING
*        goodsmvt_headret    = vg_invoicedocnumber_migo
*      TABLES
*        return              = t_return_vt.
*    ENDIF.

*    IF t_return_vt[] IS INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          wait = c_x.
*      UPDATE zsdt0001
*           SET doc_material_e      = ''
*               ano_material_e      = ''
*         WHERE ch_referencia = p_zsdt0001-ch_referencia.
*    ELSE.
*      "gravar log
*      READ TABLE t_return_vt WITH KEY type = 'E'.
*      IF sy-subrc EQ 0.
*        REFRESH ti_zlest0100.
*        CLEAR vl_ponteiro.
*        SELECT  MAX( cont )
*         FROM zlest0100
*         INTO vl_ponteiro
*        WHERE ch_referencia = p_zsdt0001-ch_referencia.
*
*        IF sy-subrc = 0.
*          ADD 1 TO vl_ponteiro.
*        ELSE.
*          vl_ponteiro = 1.
*        ENDIF.
*        LOOP AT t_return_vt.
*          wa_zlest0100-mandt      = sy-mandt.
*          wa_zlest0100-ch_referencia   = p_zsdt0001-ch_referencia.
*          wa_zlest0100-msgtyp     = 'E'.
*          wa_zlest0100-msgspra    = sy-langu.
*          wa_zlest0100-msgid      = 'LES'.
*          wa_zlest0100-msgnr      = '000'.
*          wa_zlest0100-msgv1      = t_return_vt-message.
*          wa_zlest0100-data       = sy-datum.
*          wa_zlest0100-hora       = sy-uzeit.
*          wa_zlest0100-usuario    = sy-uname.
*          wa_zlest0100-cont       = vl_ponteiro.
*
*          APPEND wa_zlest0100 TO ti_zlest0100.
*          ADD 1 TO vl_ponteiro.
*        ENDLOOP.
*        MODIFY zlest0100 FROM TABLE ti_zlest0100.
*      ENDIF.

*-#133089-21.02.2024-JT-inicio
*    CASE vg_faturamento_autom.
*      WHEN abap_off.
*      WHEN abap_true.
*        DATA: t_erro2  TYPE bapiret2_t.
*        t_erro2[] = t_return_vt[].
*        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_tab_bapiret2 = t_erro2 i_status = 'REME' ).
*    ENDCASE.
*-#133089-21.02.2024-JT-fim
*    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_ENTRADA_RESIDUO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_entrada_residuo USING sl_zsdt0001        TYPE zsdt0001
                             vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc
                             vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year
                             p_retent.

  DATA: sl_zsdt0001_ent TYPE zsdt0001,
        v_nr_romaneio   TYPE zsdt0001-nr_romaneio,
        w_msn           TYPE type_msn,
        msg_text        TYPE string,
        msg_no          TYPE t100-msgnr.


  CHECK sl_zsdt0001-id_carga IS INITIAL.

  CLEAR: t_goodsmvt_item, t_return.

  CLEAR: vl_mat_doc, vl_matdocumentyear.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
  WHERE ch_referencia EQ @sl_zsdt0001-ch_referencia.

  IF ( sy-subrc = 0 ) AND
     ( _wl_0001-doc_material_e IS NOT INITIAL ) AND
     ( _wl_0001-ano_material_e IS NOT INITIAL ).
    vl_mat_doc         = _wl_0001-doc_material_e.
    vl_matdocumentyear = _wl_0001-ano_material_e.
    EXIT.
  ENDIF.

  wa_goodsmvt_header-pstng_date = sy-datum.
  wa_goodsmvt_header-doc_date   = sy-datum.
  wa_goodsmvt_header-header_txt = sl_zsdt0001-vbeln.

  wa_code-gm_code               = c_05.

*--> 19.06.2023 - Migration S4 – MIGNOW - Start
  "  wa_goodsmvt_item-material     = s_vbap-matnr.
  DATA(v_len1) = strlen( s_vbap-matnr ).
  IF v_len1 > 18.
    wa_goodsmvt_item-material_long = s_vbap-matnr .
  ELSE.
    wa_goodsmvt_item-material = s_vbap-matnr .
  ENDIF.
*<-- 19.06.2023 - Migration S4 – MIGNOW – End
  wa_goodsmvt_item-plant        = s_vbap-vstel.
  wa_goodsmvt_item-stge_loc	    = s_vbap-lgort.
  wa_goodsmvt_item-batch        = s_vbap-charg.

  wa_goodsmvt_item-move_type    = sl_zmmt0074-bwart. "ALRS 22/05/2017
  IF sl_zmmt0074-entrada_rom = 'S'. "Checa romaneio de entrada

    IF ( sl_zsdt0001-id_referencia IS INITIAL ) AND ( sl_zsdt0001-id_carga IS INITIAL ). "
      EXIT. "Não gera  doc. entrada
    ENDIF.

    IF sl_zsdt0001-id_carga IS NOT INITIAL.
      CLEAR sl_zsdt0001_ent-peso_liq.

      SELECT SUM( peso_liq )
        FROM zsdt0001
        INTO sl_zsdt0001_ent-peso_liq
       WHERE tp_movimento EQ 'E'
         AND id_carga     EQ sl_zsdt0001-id_carga.

    ELSE.

      DATA(_nr_romaneio) = sl_zsdt0001-id_referencia.

      CLEAR: sl_zsdt0001_ent.

      SELECT SINGLE *
        FROM zsdt0001
        INTO sl_zsdt0001_ent
        WHERE bukrs         = sl_zsdt0001-bukrs
        AND   branch        = sl_zsdt0001-branch
        AND   tp_movimento  = 'E'
        AND   nr_romaneio   = _nr_romaneio
        "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
      AND   nr_safra      = sl_zsdt0001-nr_safra.

      IF sl_zsdt0001_ent-ch_refer_ent IS NOT INITIAL.
        CLEAR sl_zsdt0001_ent-peso_liq.
        SELECT SUM( peso_liq )
            FROM zsdt0001
            INTO sl_zsdt0001_ent-peso_liq
            WHERE bukrs         = sl_zsdt0001_ent-bukrs
            AND   branch        = sl_zsdt0001_ent-branch
            AND   tp_movimento  = 'E'
            AND   ch_refer_ent  = sl_zsdt0001_ent-ch_refer_ent
            "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
        AND   nr_safra      = sl_zsdt0001_ent-nr_safra.
      ENDIF.

    ENDIF.

    IF sy-subrc = 0.

      IF sl_zsdt0001-peso_liq_comercial IS INITIAL.
        CONCATENATE 'Peso liquido comercial do romaneio' sl_zsdt0001-nr_romaneio 'não encontrado!' INTO msg_text SEPARATED BY space.
        w_msn-nr_romaneio = sl_zsdt0001-nr_romaneio.
        w_msn-messagem = msg_text.
        w_msn-tp_msn   = 'E'.
        APPEND w_msn TO t_msn.
        p_retent = 'X'.
        EXIT.
      ENDIF.

      IF  sl_zsdt0001-peso_liq GT sl_zsdt0001-peso_liq_comercial.  "SL_ZSDT0001_ENT-PESO_LIQ.
        wa_goodsmvt_item-entry_qnt = sl_zsdt0001-peso_liq - sl_zsdt0001-peso_liq_comercial.
      ELSE.
        EXIT. "Não gera  doc. entrada
      ENDIF.
    ELSE.
      CONCATENATE 'Não foi encontrado o romaneio de entrada, nesta data' '!' INTO msg_text SEPARATED BY space.
      w_msn-nr_romaneio = sl_zsdt0001-nr_romaneio.
      w_msn-messagem = msg_text.
      w_msn-tp_msn   = 'E'.
      APPEND w_msn TO t_msn.
      p_retent = 'X'.
      EXIT.
    ENDIF.

  ELSE.

    wa_goodsmvt_item-entry_qnt    = sl_zsdt0001-peso_liq.

  ENDIF.

  APPEND wa_goodsmvt_item TO t_goodsmvt_item.

*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header  = wa_goodsmvt_header
*      goodsmvt_code    = wa_code
*    IMPORTING
*      materialdocument = vl_mat_doc
*      matdocumentyear  = vl_matdocumentyear
*    TABLES
*      goodsmvt_item    = t_goodsmvt_item
*      return           = t_return.

  READ TABLE t_return WITH KEY type = 'E'.

  IF ( NOT sy-subrc IS INITIAL ) AND
     ( vl_mat_doc IS NOT INITIAL ) AND
     ( vl_matdocumentyear IS NOT INITIAL ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

*    UPDATE zsdt0001 SET doc_material_e = vl_mat_doc
*                        ano_material_e = vl_matdocumentyear
*     WHERE ch_referencia EQ sl_zsdt0001-ch_referencia.

    COMMIT WORK.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    LOOP AT t_return INTO DATA(w_return).
      WRITE w_return-number TO msg_no.
      CALL FUNCTION 'MESSAGE_PREPARE'
        EXPORTING
          msg_id                 = w_return-id
          msg_no                 = msg_no
          msg_var1               = w_return-message_v1
          msg_var2               = w_return-message_v2
          msg_var3               = w_return-message_v3
          msg_var4               = w_return-message_v4
        IMPORTING
          msg_text               = msg_text
        EXCEPTIONS
          function_not_completed = 1
          message_not_found      = 2
          OTHERS                 = 3.
      w_msn-messagem = msg_text.
      w_msn-tp_msn   = w_return-type.
      APPEND w_msn TO t_msn.
    ENDLOOP.

    p_retent = 'X'.

  ENDIF.

ENDFORM.                          " Z_ENTRADA_RESIDUO

*&---------------------------------------------------------------------*
*&      Form  MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SL_DATA_REM  text
*----------------------------------------------------------------------*
FORM memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI

FORM z_add_itens_rom TABLES p_item     STRUCTURE bapidlvreftosalesorder
                      USING p_zsdt0001 TYPE zsdt0001.

  DATA: s_item TYPE bapidlvreftosalesorder.

  CLEAR: p_item[].

  LOOP AT t_zsdt0001_item_grp WHERE ch_referencia = p_zsdt0001-ch_referencia.

    CLEAR: s_item.

    READ TABLE tg_vbap WITH KEY vbeln = t_zsdt0001_item_grp-vbeln
                                posnr = t_zsdt0001_item_grp-posnr.

    CHECK sy-subrc = 0.

    s_item-ref_doc    = t_zsdt0001_item_grp-vbeln.
    s_item-ref_item   = t_zsdt0001_item_grp-posnr.

    LOOP AT t_zsdt0001_item WHERE ch_referencia = t_zsdt0001_item_grp-ch_referencia
                              AND part_lote     = t_zsdt0001_item_grp-part_lote.
      ADD t_zsdt0001_item-lfimg TO s_item-dlv_qty.
    ENDLOOP.

    s_item-sales_unit = tg_vbap-vrkme.

    APPEND s_item TO p_item.

  ENDLOOP.

ENDFORM.

FORM z_ins_lote_itens TABLES p_return   STRUCTURE bapiret2
                       USING p_delivery TYPE bapishpdelivnumb-deliv_numb.

  DATA: wa_header_data    TYPE bapiobdlvhdrchg,
        wa_header_control TYPE bapiobdlvhdrctrlchg,
        header_partner    TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE,
        item_data         TYPE TABLE OF bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
        item_control      TYPE TABLE OF bapiobdlvitemctrlchg INITIAL SIZE 0 WITH HEADER LINE,
        item_data_spl     TYPE TABLE OF /spe/bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
        st_header         TYPE thead,
        st_lines          TYPE tline,
        tg_lips_aux       TYPE TABLE OF lips WITH HEADER LINE,
        tg_lips           TYPE TABLE OF lips WITH HEADER LINE.

  CLEAR: wa_header_data, wa_header_control,
         header_partner, header_partner[],
         item_data, item_data[],
         item_control, item_control[],
         item_data_spl, item_data_spl[],
         p_return[].

  DATA: vl_itm_lote      TYPE i,
        vl_count_itm     TYPE i,
        vl_particao_lote TYPE c,
        vl_lfimg         TYPE zsdt0001_item-lfimg.

  wa_header_data-deliv_numb    = p_delivery.
  wa_header_control-deliv_numb = 'X'.

  SELECT *
    FROM lips INTO TABLE tg_lips
  WHERE vbeln = p_delivery.

  tg_lips_aux[] = tg_lips[].

  "Itens
  vl_itm_lote = 900001.
  LOOP AT t_zsdt0001_item_grp WHERE vbeln  = p_vbeln-low.

    CLEAR: vl_particao_lote, vl_count_itm, tg_lips, tg_lips_aux, vl_lfimg.

    "Check se item deverá ter partição de Lote
    LOOP AT t_zsdt0001_item WHERE ch_referencia = t_zsdt0001_item_grp-ch_referencia
                              AND part_lote     = t_zsdt0001_item_grp-part_lote.
      ADD t_zsdt0001_item-lfimg TO vl_lfimg.
      ADD 1 TO vl_count_itm.
    ENDLOOP.

    IF vl_count_itm > 1.
      vl_particao_lote = 'X'.
    ENDIF.

    "Check Item na remessa criado referente ao item da O.V
    LOOP AT tg_lips_aux WHERE vgbel  = t_zsdt0001_item_grp-vbeln
                          AND vgpos  = t_zsdt0001_item_grp-posnr
                          AND lfimg  = vl_lfimg.
      MOVE-CORRESPONDING tg_lips_aux TO tg_lips.
      DELETE tg_lips_aux.
      EXIT.
    ENDLOOP.

    item_data-deliv_numb          = p_delivery.
    item_data-hieraritem          = tg_lips-posnr.
    item_data-usehieritm          = 1.

    "Lotes
    LOOP AT t_zsdt0001_item WHERE ch_referencia = t_zsdt0001_item_grp-ch_referencia
                              AND part_lote     = t_zsdt0001_item_grp-part_lote.

      t_zsdt0001_item-posnr_rem          = tg_lips-posnr.

      IF vl_particao_lote IS NOT INITIAL.
        t_zsdt0001_item-itm_lote    = vl_itm_lote.
        item_data-deliv_item        = vl_itm_lote.
      ELSE.
        t_zsdt0001_item-itm_lote    = tg_lips-posnr.
        item_data-deliv_item        = tg_lips-posnr.
      ENDIF.
      item_data-batch               = t_zsdt0001_item-charg.
      item_data-dlv_qty             = t_zsdt0001_item-lfimg.
      item_data-dlv_qty_imunit      = t_zsdt0001_item-lfimg.
      item_data-fact_unit_nom       = 1.
      item_data-fact_unit_denom     = 1.
      item_data-gross_wt            = t_zsdt0001_item-brgew. "Peso bruto
      item_data-net_weight          = t_zsdt0001_item-ntgew. "Peso Liquido

*-->   27.07.2023 - Migration S4 – - Start
      "  item_data-material            = t_zsdt0001_item-matnr.
      DATA(v_len2) = strlen( t_zsdt0001_item-matnr ).
      IF v_len2 > 18.
        item_data-material_long = t_zsdt0001_item-matnr .
      ELSE.
        item_data-material      = t_zsdt0001_item-matnr .
      ENDIF.
*      <-- 19.06.2023 - Migration S4 – End

      APPEND item_data.

      item_control-deliv_numb       = p_delivery.
      IF vl_particao_lote IS NOT INITIAL.
        item_control-deliv_item     = t_zsdt0001_item-itm_lote.
      ELSE.
        item_control-deliv_item     = tg_lips-posnr.
      ENDIF.
      item_control-chg_delqty       = 'X'.
      item_control-volume_flg       = 'X'.
      item_control-net_wt_flg       = 'X'.
      item_control-gross_wt_flg     = 'X'.
      APPEND item_control.

      IF vl_particao_lote IS NOT INITIAL.
        ADD 1 TO vl_itm_lote.
      ENDIF.

      MODIFY t_zsdt0001_item.

    ENDLOOP.

  ENDLOOP.

*  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*    EXPORTING
*      header_data    = wa_header_data
*      header_control = wa_header_control
*      delivery       = p_delivery
*    TABLES
*      header_partner = header_partner
*      item_data      = item_data
*      item_control   = item_control
*      return         = p_return
*      item_data_spl  = item_data_spl.
*
*  IF p_return[] IS INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ENDIF.

ENDFORM.


FORM z_troca_peso TABLES p_return   STRUCTURE bapiret2
                       USING p_delivery   TYPE bapishpdelivnumb-deliv_numb
                             p_zsdt0001   TYPE zsdt0001
                             p_peso_bruto TYPE brgew_15
                             p_peso_liq   TYPE ntgew_15.

  DATA: wa_header_data    TYPE bapiobdlvhdrchg,
        wa_header_control TYPE bapiobdlvhdrctrlchg,
        header_partner    TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE,
        item_data         TYPE TABLE OF bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
        item_control      TYPE TABLE OF bapiobdlvitemctrlchg INITIAL SIZE 0 WITH HEADER LINE,
        item_data_spl     TYPE TABLE OF /spe/bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
        st_header         TYPE thead,
        st_lines          TYPE tline,
        tg_lips           TYPE TABLE OF lips WITH HEADER LINE,
        t_set             TYPE TABLE OF rgsb4,
        w_set             TYPE rgsb4.

  RANGES: r_matkl FOR vbap-matkl.

  CLEAR: wa_header_data, wa_header_control,
         header_partner, header_partner[],
         item_data, item_data[],
         item_control, item_control[],
         item_data_spl, item_data_spl[],
         p_return[].

  DATA: vl_itm_lote      TYPE i,
        vl_count_itm     TYPE i,
        vl_particao_lote TYPE c,
        vl_lfimg         TYPE zsdt0001_item-lfimg.

  CLEAR: vl_particao_lote, vl_count_itm, tg_lips, vl_lfimg.
  wa_header_data-deliv_numb    = p_delivery.
  wa_header_control-deliv_numb = 'X'.

  SELECT *
    FROM lips INTO TABLE tg_lips
  WHERE vbeln = p_delivery.

*-CS2021000615 - 20.07.2021 - JT - inicio
*------------------------------
*-Grupo de material que gera volume
*------------------------------
  FREE: t_set, r_matkl.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      class           = '0000'
      setnr           = 'ZSDI0009_MATKL'
      no_descriptions = abap_false
    TABLES
      set_values      = t_set
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  LOOP AT t_set   INTO w_set.
    r_matkl-sign   = 'I'.
    r_matkl-option = 'EQ'.
    r_matkl-low    = w_set-from.
    APPEND r_matkl.
  ENDLOOP.

*------------------------------
*-item OV
*------------------------------
  SELECT matkl, voleh
    FROM vbap
    INTO (@DATA(l_matkl), @DATA(l_voleh))
      UP TO 1 ROWS
   WHERE vbeln = @p_zsdt0001-vbeln
     AND matnr = @p_zsdt0001-matnr.
  ENDSELECT.

  IF ( l_matkl IN r_matkl[] AND r_matkl[] IS NOT INITIAL ) AND ( p_zsdt0001-matnr IS NOT INITIAL ).
*-------------------------
*---Qtd embalagens
*-------------------------
    SELECT nm_qtd_embalagens
      INTO @DATA(l_qtd_emb)
      FROM zsdt0001ovro
        UP TO 1 ROWS
*    WHERE id_carga          = @p_zsdt0001-id_carga
     WHERE nr_ordem_venda    = @p_zsdt0001-vbeln
       AND ch_referencia_sai = @p_zsdt0001-ch_referencia.
    ENDSELECT.

    IF ( sy-subrc = 0 ) AND ( l_qtd_emb IS NOT INITIAL  ).
      item_data-volume     = l_qtd_emb.
      item_data-volumeunit = l_voleh.
    ELSE.
      SELECT SINGLE * INTO @DATA(lwa_zsdt0001_item)
        FROM zsdt0001_item
       WHERE ch_referencia  = @p_zsdt0001-ch_referencia
         AND vbeln          = @p_zsdt0001-vbeln
         AND matnr          = @p_zsdt0001-matnr.

      IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt0001_item-voleh IS NOT INITIAL ) AND ( lwa_zsdt0001_item-volum IS NOT INITIAL ).
        item_data-volume     = lwa_zsdt0001_item-volum.
        item_data-volumeunit = lwa_zsdt0001_item-voleh.
      ENDIF.
    ENDIF.

  ENDIF.
*-CS2021000615 - 20.07.2021 - JT - fim

  READ TABLE tg_lips INDEX 1.

  item_data-deliv_numb          = p_delivery.
  item_data-hieraritem          = tg_lips-posnr.
  item_data-usehieritm          = 1.
  item_data-deliv_item          = tg_lips-posnr.
  item_data-batch               = p_zsdt0001-nr_safra.

  IF ( v_charg IS NOT INITIAL ).
    item_data-batch             = v_charg.
  ENDIF.

  IF s_vbap-charg IS NOT INITIAL.
    item_data-batch = s_vbap-charg.
  ENDIF.

  IF p_zsdt0001-qtde_remessa IS NOT INITIAL.
    item_data-dlv_qty             = p_zsdt0001-qtde_remessa.
    item_data-dlv_qty_imunit      = p_zsdt0001-qtde_remessa. "P_PESO.
    item_data-fact_unit_denom     = 1.
    item_data-fact_unit_nom       = 1.
  ELSE.
    item_data-dlv_qty             = p_peso_liq.
    item_data-dlv_qty_imunit      = p_peso_liq. "P_PESO.
    item_data-fact_unit_nom       = 1.
    item_data-fact_unit_denom     = 1.
  ENDIF.

  item_data-gross_wt            = p_peso_bruto.    "P_ZSDT0001-PESO_LIQ. "Peso bruto
  item_data-net_weight          = p_peso_liq.       "P_PESO. "Peso Liquido

*-->   27.07.2023 - Migration S4 – - Start
  "  item_data-material            = p_zsdt0001-matnr.
  DATA(v_len2) = strlen( p_zsdt0001-matnr ).
  IF v_len2 > 18.
    item_data-material_long = p_zsdt0001-matnr .
  ELSE.
    item_data-material      = p_zsdt0001-matnr .
  ENDIF.
*      <-- 19.06.2023 - Migration S4 – End






  APPEND item_data.

  item_control-deliv_numb       = p_delivery.
  item_control-deliv_item       = tg_lips-posnr.
  item_control-chg_delqty       = 'X'.
  item_control-volume_flg       = 'X'.
  item_control-net_wt_flg       = 'X'.
  item_control-gross_wt_flg     = 'X'.
  APPEND item_control.


*  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
*    EXPORTING
*      header_data    = wa_header_data
*      header_control = wa_header_control
*      delivery       = p_delivery
*    TABLES
*      header_partner = header_partner
*      item_data      = item_data
*      item_control   = item_control
*      return         = p_return
*      item_data_spl  = item_data_spl.
*
*  IF p_return[] IS INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*      EXPORTING
*        wait = 'X'.
*  ENDIF.

ENDFORM.

FORM f_check_excecao_residuo USING p_matnr TYPE zsdt0001-matnr
                                   p_bukrs TYPE zsdt0001-bukrs
                          CHANGING c_ok    TYPE c.

  CLEAR: c_ok.

  DATA: lva_matnr TYPE zsdt0001-matnr.

  lva_matnr = p_matnr.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = lva_matnr
    IMPORTING
      output = lva_matnr.

  CONCATENATE p_bukrs '-' lva_matnr INTO DATA(_bukrs_mat_exc_key).

  SELECT SINGLE *
    FROM setleaf INTO @DATA(_lwa_bukrs_exc)
   WHERE setname EQ 'RESIDUO_EXC'
     AND valfrom EQ @_bukrs_mat_exc_key.

  IF sy-subrc EQ 0.
    c_ok = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form z_check_baixa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM z_check_baixa .

  DATA: tl_set_values TYPE TABLE OF rgsb4,
        lv_aufnr      TYPE resb-aufnr.

  lv_aufnr = |{ v_charg ALPHA = IN }|.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'MV45AFZZ_WERKS'
      class         = '0000'
    TABLES
      set_values    = tl_set_values
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  DATA(ls_set_values) = VALUE #( tl_set_values[ field = 'WERKS' from = s_vbap-werks ] OPTIONAL ).

  IF ls_set_values IS NOT INITIAL.
    SELECT rsnum, aufnr, enmng, erfmg
      INTO TABLE @DATA(lt_resb)
      FROM resb
      WHERE aufnr EQ @lv_aufnr.

    LOOP AT lt_resb INTO DATA(wa_resb).
      IF wa_resb-enmng NE wa_resb-erfmg.
        IF wa_resb-enmng EQ 0.
*-#133089-21.02.2024-JT-inicio
          CASE vg_faturamento_autom.
            WHEN abap_off.
              MESSAGE s024(sd) WITH 'Verificar baixa de componentes, ' 'falha de movimentação' DISPLAY LIKE 'E'.
              LEAVE LIST-PROCESSING.
            WHEN abap_true.
              MESSAGE s024(sd) WITH 'Verificar baixa de componentes, ' 'falha de movimentação' INTO DATA(l_mesg).
              lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
              lc_faturamento_automatico->set_mensagem( i_cod = '999' i_mesg = CONV #( l_mesg ) ).
          ENDCASE.
*-#133089-21.02.2024-JT-fim
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
