*&---------------------------------------------------------------------*
*& Report  ZMMR041
*&
*&---------------------------------------------------------------------*
*&TITULO: Relatório de Acompanhamento do tempo de carregamento
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 25.10.2013
*TRANSACAO: ZMM0061
*&---------------------------------------------------------------------*

REPORT  zmmr041.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon,
            slis.


*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: zsdt0001,zmmt0045, t001w, mara.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
   BEGIN OF ty_t001k,
     bwkey      TYPE t001k-bwkey,
     bukrs      TYPE t001k-bukrs,
   END OF ty_t001k,

  BEGIN OF ty_t001w,
     werks      TYPE t001w-werks,
     name1      TYPE t001w-name1,
   END OF ty_t001w,

   BEGIN OF ty_t001,
     bukrs      TYPE t001-bukrs,
     butxt      TYPE t001-butxt,
   END OF ty_t001,

   BEGIN OF ty_j_1bbranch,
    branch      TYPE j_1bbranch-branch,
    name        TYPE j_1bbranch-name,
   END OF ty_j_1bbranch,



   BEGIN OF ty_makt,
        matnr     TYPE makt-matnr,
        maktx     TYPE makt-maktx,
   END OF ty_makt,

   BEGIN OF ty_saida,
    linha           TYPE i,
    cap_correias    TYPE zmmt0045-cap_correias,
    butxt           TYPE t001-butxt,
    regional        TYPE zmmt0045-regional,
    name1           TYPE t001w-name1,
    maktx           TYPE makt-maktx,
    mes(2),
    dt_abertura     TYPE zsdt0001-dt_abertura,
    nr_romaneio     TYPE zsdt0001-nr_romaneio,
    peso_liq        TYPE zsdt0001-peso_liq,
    tp_carregamento TYPE sy-uzeit,
    tp_documentacao TYPE sy-uzeit, "Tempo documentação
    tp_total        TYPE sy-uzeit, "Tempo Total
    tempo_ult(1),
    tp_sefaz_nfe    TYPE sy-uzeit, "Validação SEFAZ NF-e
    tp_sefaz_cte    TYPE sy-uzeit, "Validação SEFAZ CT-e
    tp_tip_card     TYPE sy-uzeit, "Validação TIP CARD
    motivo_atraso   TYPE zmmt0047-motivo_atraso,
    ch_referencia   TYPE zmmt0047-ch_referencia,
    tp_padrao       TYPE zmmt0045-tp_padrao,
    color_cell      TYPE lvc_t_scol,  " Cell color
   END OF ty_saida.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: it_zsdt0001         TYPE TABLE OF zsdt0001,
      it_zmmt0045         TYPE TABLE OF zmmt0045,
      it_zmmt0047         TYPE TABLE OF zmmt0047,
      it_makt             TYPE TABLE OF ty_makt,
      it_t001k            TYPE TABLE OF ty_t001k,
      it_t001w            TYPE TABLE OF ty_t001w,
      it_t001             TYPE TABLE OF ty_t001,
      it_color            TYPE TABLE OF lvc_s_scol,
      it_saida            TYPE TABLE OF ty_saida.


*----------------------------------------------------------------------*
* WORKAREA INTERNA
*----------------------------------------------------------------------*
DATA: wa_zsdt0001         TYPE zsdt0001,
      wa_zmmt0045         TYPE zmmt0045,
      wa_zmmt0047         TYPE zmmt0047,
      wa_makt             TYPE ty_makt,
      wa_t001k            TYPE ty_t001k,
      wa_t001w            TYPE ty_t001w,
      wa_t001             TYPE ty_t001,
      wa_color            TYPE lvc_s_scol,
      wa_saida            TYPE ty_saida.


************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
************************************************************************
*Class definition for ALV toolbar

DATA: editcontainer    TYPE REF TO cl_gui_custom_container,
      cl_container     TYPE REF TO cl_gui_custom_container,
      editor           TYPE REF TO cl_gui_textedit,
      cl_container_95  TYPE REF TO cl_gui_docking_container,
      cl_container_05  TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id    TYPE REF TO cl_dd_document,
      cl_grid          TYPE REF TO cl_gui_alv_grid,
      wa_stable         TYPE lvc_s_stbl,
      wa_afield        TYPE lvc_s_fcat,
      it_fieldcat      TYPE lvc_t_fcat,
      w_fieldcat       TYPE lvc_s_fcat,
      gt_f4            TYPE lvc_t_f4 WITH HEADER LINE,
      i_sort           TYPE lvc_t_sort,
      wa_layout        TYPE lvc_s_layo,
      gd_layout        TYPE slis_layout_alv      ,
      is_stable        TYPE lvc_s_stbl VALUE 'XX',
      wg_save(1)       TYPE c,
      ok-code          TYPE sy-ucomm.

DATA: gs_variant_c TYPE disvariant.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_data_changed   FOR EVENT data_changed OF cl_gui_alv_grid
                        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.
    CLASS-METHODS:
       on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
                      IMPORTING e_modified et_good_cells.
    CLASS-METHODS:
      on_f4                      FOR EVENT onf4                 OF cl_gui_alv_grid
         IMPORTING  e_fieldname
                    es_row_no
                    er_event_data
                    et_bad_cells
                    e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION



*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.
    DATA: ls_good         TYPE lvc_s_modi,
            lv_value      TYPE lvc_value,
            vl_value      TYPE lvc_value,
            wl_zmmt0046   TYPE zmmt0046.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'MOTIVO_ATRASO'.
      lv_value = ls_good-value.

      SELECT SINGLE *
      FROM zmmt0046
      INTO wl_zmmt0046
      WHERE des_motivo =  lv_value.

      IF sy-subrc NE 0.
        CLEAR lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'MOTIVO_ATRASO'
            i_value     = lv_value.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH  'Motivo não cadastrado!'.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.
*** Método de atualização de dados na Tela
    CALL METHOD cl_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_f4.
    TYPES: BEGIN OF t_f4_structure,
               fieldtext TYPE dfies-fieldtext,
               fieldname TYPE dfies-fieldname,
            END OF t_f4_structure.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA: ls_modi TYPE lvc_s_modi.


    DATA: wl_return_lg TYPE  ddshretval ,
          wl_dselclg   TYPE  dselc  ,

          tl_return_lg TYPE TABLE OF ddshretval,
          tl_dselclg   TYPE TABLE OF dselc  .

    CASE e_fieldname.
      WHEN 'MOTIVO_ATRASO'.
        TYPES : BEGIN OF ty_zmmt0046,
                 banks        TYPE zmmt0046-des_motivo,
                END OF ty_zmmt0046.


        DATA: wl_return_chvl TYPE  ddshretval ,
              wl_dselcchvl   TYPE  dselc  ,
              tl_return_chvl TYPE TABLE OF ddshretval,
              tl_dselcchvl   TYPE TABLE OF dselc  ,
              tl_zmmt0046    TYPE TABLE OF ty_zmmt0046,
              tabix_46       TYPE sy-tabix.


        SELECT des_motivo
          FROM zmmt0046
          INTO TABLE tl_zmmt0046
          ORDER BY des_motivo.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
           EXPORTING
             retfield        = 'DES_MOTIVO'
             value_org       = 'S'
            " dynpprog        = sy-repid
            " dynpnr          = sy-dynnr
            " dynprofield     =
           TABLES
             value_tab       = tl_zmmt0046
             return_tab      = tl_return_chvl
             dynpfld_mapping = tl_dselcchvl.

        READ TABLE tl_return_chvl INTO wl_return_chvl INDEX 1.
        IF sy-subrc = 0 AND wl_return_chvl-fieldval <> ''.
          ASSIGN er_event_data->m_data->* TO <itab>.
          ls_modi-row_id    = es_row_no-row_id.
          ls_modi-fieldname = 'MOTIVO_ATRASO'.
          ls_modi-value     = wl_return_chvl-fieldval.
          APPEND ls_modi TO <itab>.

          er_event_data->m_event_handled = 'X'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.                    "ON_F4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS:
  p_werks FOR t001w-werks  ,
  p_regi  FOR zmmt0045-regional NO INTERVALS NO-EXTENSION,
  p_peri  FOR zsdt0001-dt_abertura  OBLIGATORY,
  p_matnr FOR mara-matnr.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN END   OF BLOCK a1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_regi-low.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_regi OCCURS 0,
          regional      TYPE zmmt0045-regional,
          cap_correias  TYPE zmmt0045-cap_correias,
          tp_padrao     TYPE zmmt0045-tp_padrao,
         END OF tl_regi.

  REFRESH tl_regi.

  SELECT DISTINCT regional cap_correias tp_padrao
    FROM zmmt0045
    INTO TABLE tl_regi
    ORDER BY regional.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'REGIONAL'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZMMT0045-REGIONAL'
      value_org       = 'S'
    TABLES
      value_tab       = tl_regi
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: z_seleciona_dados,
           z_processa_dados,
           z_imprime_dados.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_dados .

  IF p_regi IS NOT INITIAL.
    SELECT *
    FROM zmmt0045
    INTO TABLE it_zmmt0045
    WHERE regional IN p_regi.

    IF it_zmmt0045[] IS NOT INITIAL.
      SELECT *
       FROM zsdt0001
       INTO TABLE it_zsdt0001
       FOR ALL ENTRIES IN it_zmmt0045
       WHERE branch      EQ it_zmmt0045-werks
*       AND   dt_abertura IN p_peri
       AND   dt_movimento IN p_peri
       AND   tp_movimento = 'S'.

    ENDIF.
  ELSE.
    SELECT *
     FROM zsdt0001
     INTO TABLE it_zsdt0001
     WHERE branch      IN p_werks
     "AND   dt_abertura IN p_peri
     AND   dt_movimento IN p_peri
     AND   tp_movimento = 'S'.

    SELECT *
    FROM zmmt0045
    INTO TABLE it_zmmt0045
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE werks = it_zsdt0001-branch.
  ENDIF.

  IF it_zsdt0001[] IS INITIAL.
    EXIT.
  ENDIF.

  SELECT *
    FROM zmmt0047
    INTO TABLE it_zmmt0047
    FOR ALL ENTRIES IN it_zsdt0001
    WHERE ch_referencia = it_zsdt0001-ch_referencia
      AND matnr         IN p_matnr.

  CHECK sy-subrc = 0.

  SELECT matnr maktx
    FROM makt
    INTO TABLE it_makt
    FOR ALL ENTRIES IN it_zmmt0047
    WHERE matnr = it_zmmt0047-matnr
    AND spras = 'P'.

  SELECT bwkey bukrs
    FROM t001k
    INTO TABLE it_t001k
    FOR ALL ENTRIES IN it_zmmt0047
    WHERE bwkey = it_zmmt0047-filial.

  IF it_t001k[] IS NOT INITIAL.
    SELECT werks name1
    FROM t001w
    INTO TABLE it_t001w
    FOR ALL ENTRIES IN it_t001k
    WHERE werks = it_t001k-bwkey.
  ENDIF.

  IF it_t001k[] IS NOT INITIAL.
    SELECT bukrs butxt
      FROM t001
      INTO TABLE it_t001
      FOR ALL ENTRIES IN it_t001k
      WHERE bukrs = it_t001k-bukrs.
  ENDIF.



ENDFORM.                    " Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_processa_dados .
  SORT: it_zmmt0045     BY werks,
        it_zsdt0001     BY ch_referencia,
        it_makt         BY matnr,
        it_t001k        BY bwkey,
        it_t001w        BY werks,
        it_t001         BY bukrs,
        it_zmmt0047     BY filial dt_entrada  hr_entrada.

  DATA: lv_seconds  TYPE sytabix ,
        lv_secondsf TYPE sytabix ,
        v_hora      TYPE i,
        v_resto     TYPE i,
        v_minuto    TYPE i,
        v_segundo   TYPE i,
        c_hora(2),
        c_minuto(2),
        c_segundo(2),
        w_linha     TYPE i VALUE 0.

*-----------------------------------------------------------------------
* Totalizadores de Tempo
*-----------------------------------------------------------------------
  DATA: vtp_carregamento TYPE i VALUE 0,
        vtp_carreg_linha TYPE i VALUE 0,
        vtp_documentacao TYPE i VALUE 0,
        vtp_docume_linha TYPE i VALUE 0,
        vtp_total        TYPE i VALUE 0,
        vtp_total_hr     TYPE i,
        vtp_sefaz_nfe    TYPE i VALUE 0,
        vtp_sefaz_cte    TYPE i VALUE 0,
        vtp_sefaz_cte_l  TYPE i VALUE 0,
        vtp_tip_card     TYPE i VALUE 0.

  REFRESH it_color.
  MOVE 'TP_TOTAL' TO wa_color-fname.
  MOVE '6'         TO wa_color-color-col.
  MOVE '1'         TO wa_color-color-int.
  MOVE '1'         TO wa_color-color-inv.
  APPEND wa_color TO it_color.

  LOOP AT it_zmmt0047 INTO wa_zmmt0047.
    READ TABLE it_zsdt0001 INTO wa_zsdt0001 WITH KEY ch_referencia = wa_zmmt0047-ch_referencia BINARY SEARCH.

    READ TABLE it_t001k INTO wa_t001k WITH KEY bwkey = wa_zmmt0047-filial BINARY SEARCH.
    READ TABLE it_t001  INTO wa_t001  WITH KEY bukrs = wa_t001k-bukrs BINARY SEARCH.
    wa_saida-butxt           = wa_t001-butxt.

    CLEAR wa_zmmt0045 .
    READ TABLE it_zmmt0045 INTO wa_zmmt0045 WITH KEY  werks = wa_zsdt0001-branch BINARY SEARCH.
    IF sy-subrc = 0.
      wa_saida-cap_correias    = wa_zmmt0045-cap_correias.
      wa_saida-regional        = wa_zmmt0045-regional.
      wa_saida-tp_padrao       = wa_zmmt0045-tp_padrao.
    ENDIF.

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_zmmt0047-filial BINARY SEARCH.
    wa_saida-name1           = wa_t001w-name1.

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_zmmt0047-matnr BINARY SEARCH.
    wa_saida-maktx           = wa_makt-maktx.

*    WA_SAIDA-MES             = WA_ZSDT0001-DT_ABERTURA+4(2).
*    WA_SAIDA-DT_ABERTURA     = WA_ZSDT0001-DT_ABERTURA.

    wa_saida-mes             = wa_zsdt0001-dt_movimento+4(2).
    wa_saida-dt_abertura     = wa_zsdt0001-dt_movimento.

    wa_saida-nr_romaneio     = wa_zsdt0001-nr_romaneio.
    wa_saida-peso_liq        = wa_zsdt0001-peso_liq.

    ADD wa_zmmt0047-tp_carregamento TO vtp_carregamento.
    MOVE wa_zmmt0047-tp_carregamento TO vtp_carreg_linha.

    v_hora     = wa_zmmt0047-tp_carregamento DIV 3600.
    v_resto    = wa_zmmt0047-tp_carregamento MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO wa_saida-tp_carregamento.

    " Tempo Documentação
    CALL FUNCTION 'SWI_DURATION_DETERMINE'
      EXPORTING
        start_date = wa_zmmt0047-dt_saida
        end_date   = wa_zmmt0047-dt_sefaz_nfe
        start_time = wa_zmmt0047-hr_saida
        end_time   = wa_zmmt0047-hr_sefaz_nfe
      IMPORTING
        duration   = lv_seconds.

    " FRETE (calc separado e soma)
    CALL FUNCTION 'SWI_DURATION_DETERMINE'
      EXPORTING
        start_date = wa_zmmt0047-dt_credito
        end_date   = wa_zmmt0047-dt_cte
        start_time = wa_zmmt0047-hr_credito
        end_time   = wa_zmmt0047-hr_cte
      IMPORTING
        duration   = lv_secondsf.

    ADD lv_secondsf TO lv_seconds.
    MOVE lv_seconds TO vtp_docume_linha.

    ADD lv_seconds TO vtp_documentacao.

    v_hora     = lv_seconds DIV 3600.
    v_resto    = lv_seconds MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_documentacao. "Tempo documentação

    "Tempo Total
    "SOMA DAS COLUNAS “TEMPO DE CARREGAMENTO + TEMPO DE DOCUMENTAÇÃO”
    lv_seconds = vtp_carreg_linha + vtp_docume_linha.
*    CALL FUNCTION 'SWI_DURATION_DETERMINE'
*      EXPORTING
*        START_DATE = WA_ZMMT0047-DT_ENTRADA
*        END_DATE   = WA_ZMMT0047-DT_CREDITO
*        START_TIME = WA_ZMMT0047-HR_ENTRADA
*        END_TIME   = WA_ZMMT0047-HR_CREDITO
*      IMPORTING
*        DURATION   = LV_SECONDS.
*
    ADD lv_seconds TO vtp_total.

    vtp_total_hr = wa_saida-tp_padrao * 60.

    IF lv_seconds GT vtp_total_hr.
      wa_saida-tempo_ult = 'X'.
      wa_saida-color_cell[] = it_color[].
    ENDIF.
    v_hora     = lv_seconds DIV 3600.
    v_resto    = lv_seconds MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_total. "Tempo Total

    "Validação SEFAZ NF-e
    CALL FUNCTION 'SWI_DURATION_DETERMINE'
      EXPORTING
        start_date = wa_zmmt0047-dt_nfe
        end_date   = wa_zmmt0047-dt_sefaz_nfe
        start_time = wa_zmmt0047-hr_nfe
        end_time   = wa_zmmt0047-hr_sefaz_nfe
      IMPORTING
        duration   = lv_seconds.

    ADD lv_seconds TO vtp_sefaz_nfe.
    v_hora     = lv_seconds DIV 3600.
    v_resto    = lv_seconds MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_sefaz_nfe. "Validação SEFAZ NF-e

    "Validação SEFAZ CT-e
    CALL FUNCTION 'SWI_DURATION_DETERMINE'
      EXPORTING
        start_date = wa_zmmt0047-dt_cte
        end_date   = wa_zmmt0047-dt_sefaz_cte
        start_time = wa_zmmt0047-hr_cte
        end_time   = wa_zmmt0047-hr_sefaz_cte
      IMPORTING
        duration   = lv_seconds.

    ADD lv_seconds TO vtp_sefaz_cte.
    MOVE lv_seconds TO vtp_sefaz_cte_l.

    v_hora     = lv_seconds DIV 3600.
    v_resto    = lv_seconds MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_sefaz_cte. "Validação SEFAZ CT-e

    "ZMMT0047- DT_CREDITO + ZMMT0047- HR_CREDITO subtrai ZMMT0047- DT_SOLICITACAO + ZMMT0047-HR_SOLICITACAO subtrai Coluna Val. Sefaz CT-e


    "Validação TIP CARD (parte 1)
    CALL FUNCTION 'SWI_DURATION_DETERMINE'
      EXPORTING
        start_date = wa_zmmt0047-dt_solicitacao
        end_date   = wa_zmmt0047-dt_credito
        start_time = wa_zmmt0047-hr_solicitacao
        end_time   = wa_zmmt0047-hr_credito
      IMPORTING
        duration   = lv_seconds.

*    "parte 2
*    CALL FUNCTION 'SWI_DURATION_DETERMINE'
*      EXPORTING
*        START_DATE = WA_ZMMT0047-DT_SEFAZ_CTE
*        END_DATE   = WA_ZMMT0047-DT_CREDITO
*        START_TIME = WA_ZMMT0047-HR_SEFAZ_CTE
*        END_TIME   = WA_ZMMT0047-HR_CREDITO
*      IMPORTING
*        DURATION   = LV_SECONDSF.
*
*    ADD LV_SECONDSF TO LV_SECONDS.
    "    SUBTRACT VTP_SEFAZ_CTE_L FROM LV_SECONDS.

    ADD lv_seconds TO vtp_tip_card.
    v_hora     = lv_seconds DIV 3600.
    v_resto    = lv_seconds MOD 3600.
    v_minuto   = v_resto DIV 60.
    v_segundo  = v_resto MOD 60.

    c_hora = v_hora.
    c_minuto = v_minuto.
    c_segundo = v_segundo.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_hora
      IMPORTING
        output = c_hora.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_minuto
      IMPORTING
        output = c_minuto.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = c_segundo
      IMPORTING
        output = c_segundo.

    CONCATENATE c_hora c_minuto c_segundo INTO wa_saida-tp_tip_card. "Validação TIP CARD

    wa_saida-motivo_atraso   = wa_zmmt0047-motivo_atraso.
    wa_saida-ch_referencia   = wa_zmmt0047-ch_referencia.

    ADD 1 TO w_linha.
    wa_saida-linha = w_linha.
    APPEND wa_saida TO it_saida.
    CLEAR wa_saida.
  ENDLOOP.

  CHECK w_linha GT 0.

  " Calcula tempo médio.
  CLEAR wa_saida.

  vtp_carregamento = vtp_carregamento / w_linha.
  v_hora     = vtp_carregamento  DIV 3600.
  v_resto    = vtp_carregamento  MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO wa_saida-tp_carregamento.

  " Tempo Documentação
  lv_seconds = vtp_documentacao / w_linha.

  v_hora     = lv_seconds DIV 3600.
  v_resto    = lv_seconds MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_documentacao. "Tempo documentação

  "Tempo Total
  lv_seconds = vtp_total / w_linha.

  v_hora     = lv_seconds DIV 3600.
  v_resto    = lv_seconds MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_total. "Tempo Total
  vtp_total_hr = wa_saida-tp_total.

  "Validação SEFAZ NF-e
  lv_seconds = vtp_sefaz_nfe / w_linha.

  v_hora     = lv_seconds DIV 3600.
  v_resto    = lv_seconds MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_sefaz_nfe. "Validação SEFAZ NF-e

  "Validação SEFAZ CT-e
  lv_seconds = vtp_sefaz_cte / w_linha.

  v_hora     = lv_seconds DIV 3600.
  v_resto    = lv_seconds MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO  wa_saida-tp_sefaz_cte. "Validação SEFAZ CT-e

  "Validação TIP CARD
  lv_seconds = vtp_tip_card / w_linha.

  v_hora     = lv_seconds DIV 3600.
  v_resto    = lv_seconds MOD 3600.
  v_minuto   = v_resto DIV 60.
  v_segundo  = v_resto MOD 60.

  c_hora = v_hora.
  c_minuto = v_minuto.
  c_segundo = v_segundo.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_hora
    IMPORTING
      output = c_hora.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_minuto
    IMPORTING
      output = c_minuto.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = c_segundo
    IMPORTING
      output = c_segundo.

  CONCATENATE c_hora c_minuto c_segundo INTO wa_saida-tp_tip_card. "Validação TIP CARD

  wa_saida-maktx = 'Tempos Médio'.
  ADD 1 TO w_linha.
  wa_saida-linha = w_linha.
  APPEND wa_saida TO it_saida.

  SORT it_saida BY linha.
*  LOOP AT IT_SAIDA INTO WA_SAIDA.
*    IF WA_SAIDA-TP_TOTAL GT VTP_TOTAL_HR.
*      WA_SAIDA-TEMPO_ULT = 'X'.
*      WA_SAIDA-COLOR_CELL[] = IT_COLOR[].
*      MODIFY IT_SAIDA FROM WA_SAIDA INDEX SY-TABIX TRANSPORTING TEMPO_ULT COLOR_CELL.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " Z_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_imprime_dados .

  PERFORM f_alv_fieldcat.

  wa_layout-zebra      = 'X'.
*  wa_layout-cell_merge          = 'X'.    "Desneccessário
  wa_layout-no_rowmove = 'X'.
  wa_layout-no_rowins  = 'X'.
  wa_layout-no_rowmark = space.

  wa_layout-grid_title = 'Tempos de Carregamento'.

  wa_layout-sel_mode   = 'A'.
  wa_layout-cwidth_opt = 'X'.
  wa_layout-ctab_fname = 'COLOR_CELL'.

  CALL SCREEN 0100.
ENDFORM.                    " Z_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_fieldcat .

  REFRESH it_fieldcat.
  DATA i TYPE i.
  wa_afield-tabname     = 'IT_SAIDA'.
  wa_afield-colddictxt = 'M'.
  wa_afield-selddictxt = 'M'.
  wa_afield-tipddictxt = 'M'.
  wa_afield-col_opt = 'X'.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'CAP_CORREIAS'.
  wa_afield-scrtext_m = 'Capacidade TON/H'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'BUTXT'.
  wa_afield-scrtext_m = 'Empresa'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'REGIONAL'.
  wa_afield-scrtext_m = 'Regional'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NAME1'.
  wa_afield-scrtext_m = 'Filial'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MAKTX'.
  wa_afield-scrtext_m = 'Produto'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MES'.
  wa_afield-scrtext_m = 'Mês'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'DT_ABERTURA'.
  wa_afield-scrtext_m = 'Data Movimento'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'NR_ROMANEIO'.
  wa_afield-scrtext_m = 'Romaneio'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'PESO_LIQ'.
  wa_afield-scrtext_m = 'Peso liquido'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_CARREGAMENTO'.
  wa_afield-scrtext_m = 'Tempo de carregamento'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_DOCUMENTACAO'.
  wa_afield-scrtext_m = 'Tempo de documentação'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_TOTAL'.
  wa_afield-scrtext_m = 'Tempo Total'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TEMPO_ULT'.
  wa_afield-scrtext_m = 'SUP'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_SEFAZ_NFE'.
  wa_afield-scrtext_m = 'Val.SEFAZ NF-e'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.


  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_SEFAZ_CTE'.
  wa_afield-scrtext_m = 'Val.SEFAZ CT-e'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'TP_TIP_CARD'.
  wa_afield-scrtext_m = 'Val.TIP CARD'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = ''.
  wa_afield-key           = ''.
  APPEND wa_afield TO it_fieldcat.

  i = i + 1.
  CLEAR wa_afield.
  wa_afield-col_opt = 'X'.
  wa_afield-col_pos       = i.
  wa_afield-fieldname     = 'MOTIVO_ATRASO'.
  wa_afield-scrtext_m = 'Motivo do atraso'.
  wa_afield-scrtext_l = wa_afield-scrtext_m.
  wa_afield-scrtext_s = wa_afield-scrtext_m.
  wa_afield-edit          = 'X'.
  wa_afield-f4availabl    = 'X'.
  wa_afield-key           = ''.
  wa_afield-outputlen        = 30.
  APPEND wa_afield TO it_fieldcat.

ENDFORM.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  SET PF-STATUS 'Z001' EXCLUDING fcode.
  SET TITLEBAR  'Z001'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF NOT cl_grid IS INITIAL.

    CALL METHOD cl_grid->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = it_fieldcat[].
    CALL METHOD cl_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fieldcat[].

    PERFORM zf_alv_header.
    CALL METHOD cl_grid->refresh_table_display.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ELSE.
    CREATE OBJECT obj_dyndoc_id
      EXPORTING
*    STYLE  =
*    BACKGROUND_COLOR =
*    BDS_STYLESHEET =
        no_margins = 'X'.

    PERFORM zf_alv_header .


    IF editcontainer IS INITIAL .
      CREATE OBJECT editcontainer
        EXPORTING
          container_name = 'HEADER'.
    ENDIF .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = editcontainer
      EXCEPTIONS
        html_display_error = 1.


    CREATE OBJECT cl_grid
      EXPORTING
        i_parent = cl_container_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    wa_stable-row        = 'X'.

    wg_save = 'X'.
    " WA_LAYOUT-INFO_FNAME    = 'LINE_COLOR'.

    gs_variant_c-report      = sy-repid.

    CALL METHOD cl_grid->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant_c
        is_layout       = wa_layout
        i_save          = wg_save
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = it_fieldcat[]
        it_sort         = i_sort[]
        it_outtab       = it_saida[].

    CALL METHOD cl_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH gt_f4.
    gt_f4-fieldname = 'MOTIVO_ATRASO'.
    gt_f4-register = 'X'.
    gt_f4-getbefore = 'X'.
    gt_f4-chngeafter ='X'.
    APPEND gt_f4.

    CALL METHOD cl_grid->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER:
             lcl_event_handler=>on_f4 FOR cl_grid,
             lcl_event_handler=>on_data_changed_finished FOR cl_grid,
             lcl_event_handler=>on_data_changed FOR cl_grid.


  ENDIF.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_alv_header .
  DATA:   wl_data(10),
            wl_hora(8),
            wl_linha(60),
            wl_text TYPE sdydo_text_element.

  wl_text = 'Tempos de Carregamento'.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  IF p_werks IS NOT INITIAL.
    IF p_werks-high IS INITIAL.
      CONCATENATE 'Centro  :' p_werks-low
       INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Centro' p_werks-low 'à' p_werks-high
      INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*        SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  ENDIF.

  IF p_peri IS NOT INITIAL.
    CONCATENATE p_peri-low+6(2) p_peri-low+4(2) p_peri-low+0(4) INTO  wl_data SEPARATED BY '.'.
    IF p_peri-high IS INITIAL.
      CONCATENATE 'Periodo:' wl_data
      INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE 'Periodo:' wl_data  INTO wl_linha SEPARATED BY space.
      CONCATENATE p_peri-high+6(2) p_peri-high+4(2) p_peri-high+0(4) INTO  wl_data SEPARATED BY '.'.
      CONCATENATE wl_linha 'à' wl_data  INTO wl_linha SEPARATED BY space.
    ENDIF.
    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text "WL_LINHA
*         SAP_STYLE    = CL_DD_AREA=>HEADING
        sap_fontsize = cl_dd_area=>list_normal.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  IF NOT cl_grid IS INITIAL.
    CALL METHOD cl_grid->dispatch
      EXPORTING
        cargo         = sy-ucomm
        eventid       = 19
        is_shellevent = ' '.

    IF sy-ucomm IS INITIAL.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = is_stable.
    ENDIF.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'UP'.
      REFRESH it_saida.
      CALL METHOD cl_grid->refresh_table_display.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SAVE'.
      LOOP AT it_saida INTO wa_saida.
        UPDATE zmmt0047 SET motivo_atraso = wa_saida-motivo_atraso
        WHERE ch_referencia  = wa_saida-ch_referencia .
      ENDLOOP.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
