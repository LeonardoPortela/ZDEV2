*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: CAMILA BRAND                                            &*
*& Data.....: 21/01/2022                                              &*
*& Descrição: Acompanhamento Rateio Combustível Frota Rodoviária      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zpmr0076 MESSAGE-ID zcarga.

"Tabelas
TABLES: zpmt0063, zpmt0030.


TYPES: BEGIN OF ty_saida_pend,
         pedido        TYPE  zpmt0032-pedido,
         fatura        TYPE  zpmt0032-fatura,
         cnpj          TYPE  zpmt0032-cnpj,
         dt_fatura     TYPE  zpmt0032-dt_fatura,
         cupom_fisc    TYPE  zpmt0024-cupom_fisc,
         dt_cupom_fisc TYPE  zpmt0024-dt_cupom_fisc,
         hr_cupom_fisc TYPE  zpmt0024-hr_cupom_fisc,
         placa         TYPE  zpmt0024-placa,
         ordem         TYPE  zpmt0024-ordem,
         cod_material  TYPE  zpmt0026-cod_material,
         desc_material TYPE  zpmt0026-desc_material,
         qtde          TYPE  zpmt0026-qtde,
         vlr_unt       TYPE  zpmt0026-vlr_unt,
         vlr_total     TYPE  zpmt0026-vlr_total,
       END OF ty_saida_pend.

TYPES: BEGIN OF ty_saida_proc,
         obj_key       TYPE  zpmt0063-obj_key,
         seqitem       TYPE  zpmt0063-seqitem,
         bschl         TYPE  zpmt0063-bschl,
         gsber         TYPE  zpmt0063-gsber,
         xblnr         TYPE  zpmt0063-xblnr,
         cod_material  TYPE  zpmt0063-cod_material,
         desc_material TYPE  zpmt0063-desc_material,
         bukrs         TYPE  zpmt0063-bukrs,
         bldat         TYPE  zpmt0063-bldat,
         budat         TYPE  zpmt0063-budat,
         gjahr         TYPE  zpmt0063-gjahr,
         monat         TYPE  zpmt0063-monat,
         hkont         TYPE  zpmt0063-hkont,
         wrbtr         TYPE  zpmt0063-wrbtr,
         sgtxt         TYPE  zpmt0063-sgtxt,
         aufnr         TYPE  zpmt0063-aufnr,
         waers_i       TYPE  zpmt0063-waers_i,
         dmbtr         TYPE  zpmt0063-dmbtr,
         waers_f       TYPE  zpmt0063-waers_f,
         vornr         TYPE  zpmt0063-vornr,
         belnr         TYPE  zib_contabil_chv-belnr,
         message       TYPE  zib_contabil_err-message,
       END OF ty_saida_proc.

"Tabela Interna Global
DATA: git_zpmt0030         TYPE TABLE OF zpmt0030 WITH HEADER LINE,
      git_zpmt0032         TYPE TABLE OF zpmt0032 WITH HEADER LINE,
      git_zpmt0024         TYPE TABLE OF zpmt0024 WITH HEADER LINE,
      git_zpmt0026         TYPE TABLE OF zpmt0026 WITH HEADER LINE,
      git_zpmt0063         TYPE TABLE OF zpmt0063 WITH HEADER LINE,
      git_zib_contabil_chv TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
      git_zib_contabil_err TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
      git_saida_pend       TYPE TABLE OF ty_saida_pend,
      git_saida_proc       TYPE TABLE OF ty_saida_proc,
      git_filtro           TYPE zif_screen_linha_filtro_t,
      editor               TYPE REF TO cl_gui_textedit,
      c_editor             TYPE REF TO cl_gui_custom_container.

"Objetos
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat_pend               TYPE lvc_t_fcat,
      git_fcat_proc               TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid.

"Field-Symbol
FIELD-SYMBOLS: <gfs_t001> TYPE t001.

" Classe
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA:  event_receiver   TYPE REF TO lcl_event_receiver.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:

      hotspot_click
                  FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no .

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD hotspot_click.
    CASE e_column_id.
      WHEN ''.
    ENDCASE.

    CALL METHOD gob_gui_alv_grid->refresh_table_display.

  ENDMETHOD.
ENDCLASS.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 1.
PARAMETERS: rb_pend RADIOBUTTON GROUP g1 DEFAULT 'X'  USER-COMMAND abc.
SELECTION-SCREEN COMMENT 05(15) text-002 FOR FIELD rb_pend.

SELECTION-SCREEN POSITION 24.
SELECTION-SCREEN COMMENT 20(9) text-003 FOR FIELD rb_proc.
PARAMETERS: rb_proc RADIOBUTTON GROUP g1 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-007.
SELECT-OPTIONS: p_fatura FOR zpmt0030-fatura,
                p_ch_nfe FOR zpmt0030-chave_nfe,
                p_pedido FOR zpmt0030-pedido,
                p_gsber  FOR zpmt0063-gsber,
                p_bldat  FOR zpmt0063-bldat.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fm_at_selection_screen.

START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.


*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_start_of_selection .
  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_end_of_selection .
  PERFORM fm_filtros.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_seleciona .

  DATA:   lva_tipo_movimento TYPE zpmt0058-movi_tipo_movimento.
  RANGES: rg_werks  FOR zpmt0058-poco_centro,
          rg_status FOR zpmt0058-status_processamento .


  IF rb_pend IS NOT INITIAL.

    SELECT * FROM zpmt0030
      INTO TABLE git_zpmt0030
        WHERE fatura  IN p_fatura
        AND chave_nfe IN p_ch_nfe
        AND pedido    IN p_pedido
        AND canc <> 'X'
        AND belnr NE space
        AND mblnr NE space
        AND conf_rateio EQ space.

    IF git_zpmt0030[] IS NOT INITIAL.

      SELECT * FROM zpmt0032
        INTO TABLE git_zpmt0032
        FOR ALL ENTRIES IN git_zpmt0030
         WHERE pedido EQ git_zpmt0030-pedido.


      SELECT * FROM zpmt0024
        INTO TABLE git_zpmt0024
        FOR ALL ENTRIES IN git_zpmt0032
         WHERE fatura EQ git_zpmt0032-fatura
            AND cnpj EQ git_zpmt0032-cnpj .


      SELECT * FROM zpmt0026
        INTO TABLE git_zpmt0026
        FOR ALL ENTRIES IN git_zpmt0024
         WHERE fatura EQ git_zpmt0024-fatura
            AND cnpj EQ git_zpmt0024-cnpj
            AND cupom_fisc EQ git_zpmt0024-cupom_fisc .

    ELSE.
      MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ELSE.

    SELECT * FROM zpmt0063
      INTO TABLE git_zpmt0063
        WHERE gsber  IN p_gsber
          AND bldat  IN p_bldat.

    IF git_zpmt0063[] IS NOT INITIAL.

      SELECT * FROM zib_contabil_chv
       INTO TABLE git_zib_contabil_chv
       FOR ALL ENTRIES IN git_zpmt0063
        WHERE obj_key EQ git_zpmt0063-obj_key.

      SELECT * FROM zib_contabil_err
        INTO TABLE git_zib_contabil_err
        FOR ALL ENTRIES IN git_zpmt0063
        WHERE obj_key EQ git_zpmt0063-obj_key.

    ELSE.
      MESSAGE 'Dados não encontrados.' TYPE 'S' DISPLAY LIKE 'E'.
      LEAVE LIST-PROCESSING.
    ENDIF.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_dados_processa .

  IF rb_pend IS NOT INITIAL.

    DATA: lwa_saida_pend TYPE ty_saida_pend.

*    LOOP AT git_zpmt0030 INTO DATA(lwa_zpmt0030).
*
*      READ TABLE git_zpmt0032 INTO DATA(lwa_zpmt0032) WITH KEY pedido = lwa_zpmt0030-pedido.
*
*
*      READ TABLE git_zpmt0024 INTO DATA(lwa_zpmt0024) WITH KEY fatura = lwa_zpmt0032-fatura
*                                                                 cnpj = lwa_zpmt0032-cnpj .

    SORT: git_zpmt0026 BY fatura
                          cnpj
                          cupom_fisc.


    LOOP AT git_zpmt0026 INTO DATA(lwa_zpmt0026).

      READ TABLE git_zpmt0024 INTO DATA(lwa_zpmt0024) WITH KEY fatura = lwa_zpmt0026-fatura
                                                                 cnpj = lwa_zpmt0026-cnpj
                                                                 cupom_fisc = lwa_zpmt0026-cupom_fisc.

      READ TABLE git_zpmt0032 INTO DATA(lwa_zpmt0032) WITH KEY fatura = lwa_zpmt0024-fatura
                                                                 cnpj = lwa_zpmt0024-cnpj.

      lwa_saida_pend-pedido        =  lwa_zpmt0032-pedido.
      lwa_saida_pend-fatura        =  lwa_zpmt0032-fatura.
      lwa_saida_pend-cnpj          =  lwa_zpmt0032-cnpj.
      lwa_saida_pend-dt_fatura     =  lwa_zpmt0032-dt_fatura.
      lwa_saida_pend-cupom_fisc    =  lwa_zpmt0024-cupom_fisc.
      lwa_saida_pend-dt_cupom_fisc =  lwa_zpmt0024-dt_cupom_fisc.
      lwa_saida_pend-hr_cupom_fisc =  lwa_zpmt0024-hr_cupom_fisc.
      lwa_saida_pend-placa         =  lwa_zpmt0024-placa.
      lwa_saida_pend-ordem         =  lwa_zpmt0024-ordem.
      lwa_saida_pend-cod_material  =  lwa_zpmt0026-cod_material.
      lwa_saida_pend-desc_material =  lwa_zpmt0026-desc_material.
      lwa_saida_pend-qtde          =  lwa_zpmt0026-qtde.
      lwa_saida_pend-vlr_unt       =  lwa_zpmt0026-vlr_unt.
      lwa_saida_pend-vlr_total     =  lwa_zpmt0026-vlr_total .

      APPEND lwa_saida_pend TO git_saida_pend.


      CLEAR:  lwa_saida_pend,
              lwa_zpmt0026,
              lwa_zpmt0024,
              lwa_zpmt0032.
      "lwa_zpmt0030.

    ENDLOOP.


  ELSE.

    DATA: lwa_saida_proc TYPE ty_saida_proc.

    LOOP AT git_zpmt0063 INTO DATA(lwa_zpmt0063).

      READ TABLE git_zib_contabil_chv INTO DATA(lwa_zib_contabil_chv) WITH KEY obj_key = lwa_zpmt0063-obj_key.
      READ TABLE git_zib_contabil_err INTO DATA(lwa_zib_contabil_err) WITH KEY obj_key = lwa_zpmt0063-obj_key.

      lwa_saida_proc-obj_key       = lwa_zpmt0063-obj_key.
      lwa_saida_proc-seqitem       = lwa_zpmt0063-seqitem.
      lwa_saida_proc-bschl         = lwa_zpmt0063-bschl.
      lwa_saida_proc-gsber         = lwa_zpmt0063-gsber.
      lwa_saida_proc-xblnr         = lwa_zpmt0063-xblnr.
      lwa_saida_proc-cod_material  = lwa_zpmt0063-cod_material.
      lwa_saida_proc-desc_material = lwa_zpmt0063-desc_material.
      lwa_saida_proc-bukrs         = lwa_zpmt0063-bukrs.
      lwa_saida_proc-bldat         = lwa_zpmt0063-bldat.
      lwa_saida_proc-budat         = lwa_zpmt0063-budat.
      lwa_saida_proc-gjahr         = lwa_zpmt0063-gjahr.
      lwa_saida_proc-monat         = lwa_zpmt0063-monat.
      lwa_saida_proc-hkont         = lwa_zpmt0063-hkont.
      lwa_saida_proc-wrbtr         = lwa_zpmt0063-wrbtr.
      lwa_saida_proc-sgtxt         = lwa_zpmt0063-sgtxt.
      lwa_saida_proc-aufnr         = lwa_zpmt0063-aufnr.
      lwa_saida_proc-waers_i       = lwa_zpmt0063-waers_i.
      lwa_saida_proc-dmbtr         = lwa_zpmt0063-dmbtr.
      lwa_saida_proc-waers_f       = lwa_zpmt0063-waers_f.
      lwa_saida_proc-vornr         = lwa_zpmt0063-vornr.
      lwa_saida_proc-belnr         = lwa_zib_contabil_chv-belnr.
      lwa_saida_proc-message       = lwa_zib_contabil_err-message.

      APPEND lwa_saida_proc TO git_saida_proc.

      CLEAR: lwa_saida_proc,
             lwa_zpmt0063,
             lwa_zib_contabil_chv,
             lwa_zib_contabil_err.
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_AT_SELECTION_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_at_selection_screen .

  LOOP AT SCREEN.
    CASE abap_true.
      WHEN rb_pend.
        IF ( screen-name =  'P_BLDAT-LOW'  OR
          screen-name   =  'P_BLDAT-HIGH' OR
          screen-name   =  '%_P_BLDAT_%_APP_%-TEXT' OR
          screen-name   =  '%_P_BLDAT_%_APP_%-OPTI_PUSH' OR
          screen-name   =  '%_P_BLDAT_%_APP_%-TO_TEXT' OR
          screen-name   =  '%_P_BLDAT_%_APP_%-VALU_PUSH'  OR
          screen-name =  'P_GSBER-LOW'  OR
          screen-name   =  'P_GSBER-HIGH' OR
          screen-name   =  '%_P_GSBER_%_APP_%-TEXT' OR
          screen-name   =  '%_P_GSBER_%_APP_%-OPTI_PUSH' OR
          screen-name   =  '%_P_GSBER_%_APP_%-TO_TEXT' OR
          screen-name   =  '%_P_GSBER_%_APP_%-VALU_PUSH' ).

          screen-input  =  '0'.
          screen-active =  '0'.
          REFRESH p_bldat.
          REFRESH p_gsber.
          MODIFY SCREEN.
        ENDIF.
      WHEN rb_proc.
        IF (  screen-name =  'P_FATURA-LOW'  OR
           screen-name   =  'P_FATURA-HIGH' OR
           screen-name   =  '%_P_FATURA_%_APP_%-TEXT' OR
           screen-name   =  '%_P_FATURA_%_APP_%-OPTI_PUSH' OR
           screen-name   =  '%_P_FATURA_%_APP_%-TO_TEXT' OR
           screen-name   =  '%_P_FATURA_%_APP_%-VALU_PUSH'     OR
           screen-name   =  'P_CH_NFE-LOW'  OR
           screen-name   =  'P_CH_NFE-HIGH' OR
           screen-name   =  '%_P_CH_NFE_%_APP_%-TEXT' OR
           screen-name   =  '%_P_CH_NFE_%_APP_%-OPTI_PUSH' OR
           screen-name   =  '%_P_CH_NFE_%_APP_%-TO_TEXT' OR
           screen-name   =  '%_P_CH_NFE_%_APP_%-VALU_PUSH'   OR
           screen-name   =  'P_PEDIDO-LOW'  OR
           screen-name   =  'P_PEDIDO-HIGH' OR
           screen-name   =  '%_P_PEDIDO_%_APP_%-TEXT' OR
           screen-name   =  '%_P_PEDIDO_%_APP_%-OPTI_PUSH' OR
           screen-name   =  '%_P_PEDIDO_%_APP_%-TO_TEXT' OR
           screen-name   =  '%_P_PEDIDO_%_APP_%-VALU_PUSH' )  .


          screen-input  =  '0'.
          screen-active =  '0'.
          REFRESH p_ch_nfe.
          REFRESH p_fatura.
          REFRESH p_pedido.
          MODIFY SCREEN.

        ELSE.
          IF ( screen-name =  'P_BLDAT-LOW'  OR
                     screen-name   =  'P_BLDAT-HIGH' OR
                     screen-name   =  '%_P_BLDAT_%_APP_%-TEXT' OR
                     screen-name   =  '%_P_BLDAT_%_APP_%-OPTI_PUSH' OR
                     screen-name   =  '%_P_BLDAT_%_APP_%-TO_TEXT' OR
                     screen-name   =  '%_P_BLDAT_%_APP_%-VALU_PUSH'  OR
                     screen-name   =  'P_GSBER-LOW'  OR
                     screen-name   =  'P_GSBER-HIGH' OR
                     screen-name   =  '%_P_GSBER_%_APP_%-TEXT' OR
                     screen-name   =  '%_P_GSBER_%_APP_%-OPTI_PUSH' OR
                     screen-name   =  '%_P_GSBER_%_APP_%-TO_TEXT' OR
                     screen-name   =  '%_P_GSBER_%_APP_%-VALU_PUSH' ).

            screen-input  =  '1'.
            screen-active =  '1'.

            REFRESH p_ch_nfe.
            REFRESH p_fatura.
            REFRESH  p_pedido.
            MODIFY SCREEN.

          ENDIF.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA fcode TYPE TABLE OF sy-ucomm.

  SET PF-STATUS 'PF0100' EXCLUDING fcode.
  SET TITLEBAR  'TB0100' WITH 'Acomp. Rateio Comb. Frota Rodoviária'.

  PERFORM fm_criar_objetos.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FM_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_criar_objetos.

  DATA: lva_data(22)  TYPE c,
        lva_status_op TYPE string.

  PERFORM fm_cria_fieldcat.

  IF rb_pend IS NOT INITIAL.
    lva_status_op = 'Pendente'.
  ELSE.
    lva_status_op = 'Processados'.
  ENDIF.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
     i_titulo  = lva_status_op
     i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
   CHANGING
     alv = gob_gui_alv_grid
   )
   EQ abap_true.

    IF rb_pend IS NOT INITIAL.
      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        CHANGING
          it_outtab                     = git_saida_pend
          it_fieldcatalog               = git_fcat_pend
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.

    ELSE.
      CALL METHOD gob_gui_alv_grid->set_table_for_first_display
        CHANGING
          it_outtab                     = git_saida_proc
          it_fieldcatalog               = git_fcat_proc
        EXCEPTIONS
          invalid_parameter_combination = 1
          program_error                 = 2
          too_many_lines                = 3
          OTHERS                        = 4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat.

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  IF rb_pend IS NOT INITIAL.

    git_fcat_pend = VALUE lit_fieldcat_aux(
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'PEDIDO'            coltext = 'Pedido'          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' ) "hotspot = 'X' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'FATURA'            coltext = 'Fatura'          col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'CNPJ'              coltext = 'CNPJ'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'DT_FATURA'         coltext = 'Data Fatura'     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0032' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'CUPOM_FISC'        coltext = 'Cupom Fiscal'    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'DT_CUPOM_FISC'     coltext = 'Data Cupom'      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'HR_CUPOM_FISC'     coltext = 'Hora Cupom'      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'PLACA'             coltext = 'Placa Veic'      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'ORDEM'             coltext = 'Ordem'           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0024' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'COD_MATERIAL'      coltext = 'Material'        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'DESC_MATERIAL'     coltext = 'Desc Material'   col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'QTDE'              coltext = 'Quantidade'      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'VLR_UNT'           coltext = 'Preço'           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0026' )
  ( tabname = 'GIT_SAIDA_PEND'  fieldname = 'VLR_TOTAL'         coltext = 'Vlr Total'       col_opt = 'X' no_zero = 'X' ref_table = 'ZPMT0026' )
   ).

  ELSE.

    git_fcat_proc = VALUE lit_fieldcat_aux(
( tabname = 'GIT_SAIDA_PROC' fieldname = 'OBJ_KEY'         coltext = 'Chave referência'               col_opt = 'X' no_zero = 'X' ref_table = 'ZPMT0063' ref_field = 'OBJ_KEY' ) "hotspot = 'X' )
( tabname = 'GIT_SAIDA_PROC' fieldname = 'SEQITEM'         coltext = 'Item'                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'BSCHL'           coltext = 'Chave de lançamento'            col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'GSBER'           coltext = 'Divisão'                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'XBLNR'           coltext = 'Nº documento de referência'     col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'COD_MATERIAL'    coltext = 'Nº do material'                 col_opt = 'X' no_zero = 'X' ref_table = 'ZPMT0063' ref_field = 'COD_MATERIAL' )
( tabname = 'GIT_SAIDA_PROC' fieldname = 'DESC_MATERIAL'   coltext = 'Texto breve de material'        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'BUKRS'           coltext = 'Empresa'                        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'BLDAT'           coltext = 'Data do documento'              col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'BUDAT'           coltext = 'Data de lançamento'             col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'GJAHR'           coltext = 'Exercício'                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'MONAT'           coltext = 'Mês do exercício'               col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'HKONT'           coltext = 'Conta do Razão'                 col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'WRBTR'           coltext = 'Total dos movimentos '          col_opt = 'X' no_zero = 'X' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'SGTXT'           coltext = 'Texto do item'                  col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'AUFNR'           coltext = 'Nº ordem'                       col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'WAERS_I'         coltext = 'Código da moeda interna'        col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'DMBTR'           coltext = 'Total dos movimentos moeda'     col_opt = 'X' no_zero = 'X' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'WAERS_F'         coltext = 'Código da moeda'                col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'VORNR'           coltext = 'Nº operação'                    col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'BELNR'           coltext = 'Documento'                      col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
( tabname = 'GIT_SAIDA_PROC' fieldname = 'MESSAGE'         coltext = 'Erro'                           col_opt = 'X' no_zero = '' ref_table = 'ZPMT0063')
).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_FILTROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_filtros.

  DATA vl_text TYPE TABLE OF textpool.

  CALL FUNCTION 'RS_TEXTPOOL_READ'
    EXPORTING
      objectname = sy-repid
      action     = 'SHOW'
      language   = sy-langu
    TABLES
      tpool      = vl_text.

  FREE: git_filtro.

*  LOOP AT SCREEN.
*    git_filtro = VALUE #(
*      ( parametro = '' valor = p_bukrs )
*      ( parametro = '' valor = p_werks )
*    ).
*  ENDLOOP.
ENDFORM.
