*&---------------------------------------------------------------------*
*& Report  ZPMR0012
*& Processo de aprovação de ordens de PM
*& Aprovar orçamento ordens PM
*&---------------------------------------------------------------------*
*&  Data          : 21.01.2015
*&  Analista      : Cleudo Ferreira
*&  Desenvolvedor : Marcos Faneli
*&---------------------------------------------------------------------*
*&  Histórico de Modificações:
*&  Enio Jesus | DEVK952359 | 30.11.2015
*&  José Godoy | DEVK970506 | 15.05.2017
*&  Anderson Oe| DEVK975352 | 26.08.2017
*&---------------------------------------------------------------------*

REPORT  zpmr0012.
TABLES: zpmr0002, ihsg, ihgns, aufk.

TYPE-POOLS: icon, col.
INCLUDE <cl_alv_control>.

** TYPES **

TYPES: BEGIN OF ty_ordens.
         INCLUDE TYPE zpmr0003.
TYPES:   rowcolor(4) TYPE c,
         cell_color  TYPE lvc_t_scol,    " Cor da Célula
         sttxt       TYPE caufvd-sttxt,
         asttx       TYPE caufvd-asttx,
         icon        TYPE c LENGTH 4,
         pltxt       TYPE iflotx-pltxt,
         mark        TYPE char1.
*         sel         type c.
TYPES : END OF ty_ordens.

TYPES: BEGIN OF ty_estrategia.
         INCLUDE TYPE zpmr0002.
TYPES iwerk TYPE equz-iwerk.
TYPES: END OF ty_estrategia,

BEGIN OF ty_saida_0120.
  INCLUDE TYPE zpmr0006.
TYPES: rowcolor(4) TYPE c,
  sttxt       TYPE caufvd-sttxt,
  asttx       TYPE caufvd-asttx,
  icon1       TYPE c LENGTH 4,
  icon        TYPE c LENGTH 4,
  cellcolor   TYPE lvc_t_scol,
  style       TYPE lvc_t_styl,
  mark        TYPE char1.
TYPES END OF ty_saida_0120.

TYPES: BEGIN OF ty_saida_0121.
TYPES: belnr          TYPE bp_v_eg-belnr.
TYPES END OF ty_saida_0121.

TYPES: BEGIN OF ty_retorno,
         mensagem       TYPE string,
         codigo_retorno TYPE string,
       END OF ty_retorno,

       BEGIN OF ty_solic_suplementacao,
         aufnr TYPE aufk-aufnr,
         user  TYPE aufk-user1,
         valor TYPE aufk-user4,
       END OF ty_solic_suplementacao,

       BEGIN OF ty_fields,
         fieldname TYPE lvc_fname,
         group1    TYPE char3,
         group2    TYPE char3,
         value     TYPE char1,
         invisible TYPE char1,
       END OF ty_fields,

       BEGIN OF ty_eban,
         banfn TYPE eban-banfn,
         menge TYPE eban-menge,
         preis TYPE eban-preis,
         matnr TYPE eban-matnr,
       END OF ty_eban,

       BEGIN OF ty_mbew,
         bwkey TYPE mbew-bwkey,
         vprsv TYPE mbew-vprsv,
         matnr TYPE mbew-matnr,
         stprs TYPE mbew-stprs,
         verpr TYPE mbew-verpr,
         preis TYPE mbew-stprs,
       END OF ty_mbew,

       BEGIN OF ty_resb,
         werks   TYPE resb-werks,
         aufnr   TYPE resb-aufnr,
         matnr   TYPE resb-matnr,
         bdmng   TYPE resb-bdmng,
         rsnum   TYPE resb-rsnum,
         preis   TYPE afvc-preis,
         vprsv   TYPE mbew-vprsv,
         p_valor TYPE p DECIMALS 2,
       END OF ty_resb,

       BEGIN OF ty_vlafvc,             "Selecão de informação operação da ordem
         aufpl TYPE afvc-aufpl,
*         OBJNR TYPE AFVC-OBJNR,
         vornr TYPE afvc-vornr,
         werks TYPE afvc-werks,
*         BANFN TYPE AFVC-BANFN,
         preis TYPE afvc-preis,
         aufnr TYPE afko-aufnr,
       END OF ty_vlafvc,

       BEGIN OF ty_vlresb,
         werks TYPE resb-werks,
         aufnr TYPE resb-aufnr,
         matnr TYPE resb-matnr,
         bdmng TYPE resb-bdmng,
         stprs TYPE mbew-stprs,
         verpr TYPE mbew-verpr,
         preis TYPE afvc-preis,
         vprsv TYPE mbew-vprsv,
         vlrto TYPE afvc-preis,
       END OF ty_vlresb,

       BEGIN OF ty_vlresbd,
         werks TYPE resb-werks,
         aufnr TYPE resb-aufnr,
*         MATNR TYPE RESB-MATNR,
*         BDMNG TYPE RESB-BDMNG,
*         STPRS TYPE P DECIMALS 2,
*         VERPR TYPE P DECIMALS 2,
         preis TYPE afvc-preis,
*         VPRSV TYPE MBEW-VPRSV,
*         VLRTO TYPE P DECIMALS 2,
       END OF ty_vlresbd,

       BEGIN OF ty_afko,             "Seleção de informação do objeto
         aufnr TYPE afko-aufnr,
         aufpl TYPE afko-aufpl,
       END OF ty_afko,

       BEGIN OF ty_afvc,             "Selecão de informação operação da ordem
         aufpl TYPE afvc-aufpl,
         objnr TYPE afvc-objnr,
         vornr TYPE afvc-vornr,
         werks TYPE afvc-werks,
         banfn TYPE afvc-banfn,
         preis TYPE afvc-preis,
         aufnr TYPE afko-aufnr,
         tplnr TYPE afvc-tplnr,
         rshid TYPE afko-rshid,
         rshty TYPE afko-rshty,
       END OF ty_afvc,

       BEGIN OF ty_editor,
         line(200),
       END OF ty_editor,
*** > Início JAP - Abaco 16.05.2017
       BEGIN OF ty_zval,
         zval TYPE ztparam-zval,
       END OF ty_zval.
DATA: it_zval TYPE TABLE OF ty_zval WITH HEADER LINE.
*** < Fim JAP - Abaco 16.05.2017

** CONSTANTS **

DATA: it_afvc    TYPE TABLE OF ty_afvc,
      it_afko    TYPE TABLE OF ty_afko,
      it_resb    TYPE TABLE OF ty_resb,
      it_eban    TYPE TABLE OF ty_eban,
      it_mbew    TYPE TABLE OF ty_mbew,
      it_vlresb  TYPE TABLE OF ty_vlresb,
      it_vlresbd TYPE TABLE OF ty_vlresbd,
      it_vlafvc  TYPE TABLE OF ty_vlafvc,
      wa_afvc    TYPE ty_afvc,
      wa_afko    TYPE ty_afko,
      wa_resb    TYPE ty_resb,
      wa_eban    TYPE ty_eban,
      wa_mbew    TYPE ty_mbew,
      wa_vlresb  TYPE ty_vlresb,
      wa_vlresbd TYPE ty_vlresbd,
      wa_vlafvc  TYPE ty_vlafvc.

CONSTANTS:
  BEGIN OF c_ts_100,
    tab1 LIKE sy-ucomm VALUE 'TS_100_FC1',
    tab2 LIKE sy-ucomm VALUE 'TS_100_FC2',
  END OF c_ts_100,

  c_ordem(3)       TYPE c VALUE 'ORD',
  c_estrategia(3)  TYPE c VALUE 'EST',
  c_suplementos(3) TYPE c VALUE 'SUP',
  c_red(4)         TYPE c VALUE 'C610',
  c_liberado       TYPE c VALUE 'L',
  c_rejeitado      TYPE c VALUE 'R',
  c_tp_ordem(8)    VALUE 'TP_ORDEM',
  c_pendente       TYPE c VALUE 'P'.

** TABLES **

DATA: BEGIN OF g_ts_0100,
        subscreen   LIKE sy-dynnr,
        program     LIKE sy-repid VALUE 'ZPMR0012',
        pressed_tab LIKE sy-ucomm VALUE c_ts_100-tab1,
      END OF g_ts_0100,

      gt_ordens           TYPE TABLE OF ty_ordens,
      gt_ordens_aux       TYPE TABLE OF zpmr0003,
      gt_estrategia       TYPE TABLE OF ty_estrategia WITH HEADER LINE,
      gt_aprovadores_cc   TYPE TABLE OF zpmr0011,           "FF #185560
      zt_estrategia       TYPE TABLE OF ty_estrategia WITH HEADER LINE,
      gt_zpmr0006         TYPE TABLE OF zpmr0006,
      gt_saida_0120       TYPE TABLE OF ty_saida_0120,
      gt_fields           TYPE TABLE OF ty_fields,
      gt_ucomm            TYPE TABLE OF sy-ucomm,
      gt_editor           TYPE TABLE OF ty_editor,
      gt_fieldname        TYPE TABLE OF zstyle_fieldname,
      gw_ordens           TYPE ty_ordens,
      gw_estrategia       TYPE ty_estrategia,
      gw_editor           TYPE ty_editor,
      gw_retorno          TYPE ty_retorno,
      gw_fieldname        TYPE zstyle_fieldname,
      gw_zpmr0006         TYPE zpmr0006,
      gw_saida_0120       TYPE ty_saida_0120,
      gw_fields           TYPE ty_fields,
      lw_user_data        TYPE alm_me_user_data,
      lw_order_header     TYPE alm_me_order_header,
      lw_user_profile     TYPE alm_me_c010prf,
      obj_cont_ordem      TYPE REF TO cl_gui_custom_container,
      obj_cont_estrategia TYPE REF TO cl_gui_custom_container,
      obj_cont_0120       TYPE REF TO cl_gui_custom_container,
      obj_grid_ordem      TYPE REF TO cl_gui_alv_grid,
      obj_grid_estrategia TYPE REF TO cl_gui_alv_grid,
      obj_grid_0120       TYPE REF TO cl_gui_alv_grid,
      obj_custom_txt      TYPE REF TO cl_gui_custom_container,
      obj_custom_editor   TYPE REF TO cl_gui_textedit,
      gt_fieldcat         TYPE lvc_t_fcat,
      gw_fieldcat         TYPE lvc_s_fcat,
      gw_solic_suplem     TYPE ty_solic_suplementacao,
      gw_layout           TYPE lvc_s_layo,
      gw_variant          TYPE disvariant,
      gw_zpmr0007         TYPE zpmr0007,
      gw_stable           TYPE lvc_s_stbl,
      gw_grid_ordem       TYPE lvc_s_stbl,
      gw_grid_supl        TYPE lvc_s_stbl,
      gw_aufk             TYPE aufk,
      gw_cellcolor        TYPE lvc_s_scol,
      gt_function         TYPE ui_functions,
      gw_function         LIKE gt_function WITH HEADER LINE,
      btn_libera          TYPE char30,
      btn_rejeitar        TYPE char30,
      btn_apr_colet       TYPE char30,
      gv_registros_lines  TYPE char20,
      gt_bdcdata          TYPE STANDARD TABLE OF bdcdata,
      gw_bdcdata          LIKE LINE OF gt_bdcdata.
CONTROLS ts_0100 TYPE TABSTRIP.

FIELD-SYMBOLS: <fs_ordens> TYPE ty_ordens.

DATA:wa_color TYPE          lvc_s_scol,  " Cor para célula
     it_color TYPE TABLE OF lvc_s_scol.  " Cor para célula

** Definition

DEFINE d_preenche_fieldcat.
  CLEAR gw_fieldcat.
  gw_fieldcat-fieldname     = &1.
  gw_fieldcat-datatype      = &2.
  gw_fieldcat-coltext       = &3.
  gw_fieldcat-icon          = &4.
  gw_fieldcat-no_zero       = &5.
  gw_fieldcat-edit          = &6.
  gw_fieldcat-outputlen     = &7.
  gw_fieldcat-hotspot       = &8.
  gw_fieldcat-just          = &9.

  APPEND gw_fieldcat TO gt_fieldcat.
END-OF-DEFINITION.

DEFINE d_preenche_shdb.
  CLEAR gw_bdcdata.
  gw_bdcdata-program   = &1.
  gw_bdcdata-dynpro    = &2.
  gw_bdcdata-dynbegin  = &3.
  gw_bdcdata-fnam      = &4.
  gw_bdcdata-fval      = &5.

  REPLACE '.' WITH ',' INTO gw_bdcdata-fval.
  CONDENSE gw_bdcdata-fval NO-GAPS.

  APPEND gw_bdcdata TO gt_bdcdata.
END-OF-DEFINITION.

DEFINE d_tratar_fields.
  CLEAR gw_fields.
  gw_fields-group1    = &1.
  gw_fields-value     = &2.
  gw_fields-invisible = &3.

  APPEND gw_fields TO gt_fields .
END-OF-DEFINITION.

DEFINE d_field_style_edit.
  gw_fieldname-field = &1.
  gw_fieldname-style = &2.

  APPEND gw_fieldname TO gt_fieldname.
END-OF-DEFINITION.

** Classes
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click  FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,

      on_data_changed  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.


ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    CLEAR: gw_ordens.

    IF NOT gt_estrategia IS INITIAL.
      CLEAR gt_fields.
    ENDIF.

    CASE g_ts_0100-subscreen.
      WHEN '0110'.
        READ TABLE gt_ordens INTO gw_ordens INDEX e_row.
        PERFORM check_bloqueio USING gw_ordens-aufnr CHANGING sy-subrc.

        CHECK sy-subrc IS INITIAL.
        PERFORM set_bloqueio  USING gw_ordens-aufnr 'L'.

        IF ( gw_ordens-rowcolor EQ c_red
        AND  gt_estrategia IS NOT INITIAL ).
          d_tratar_fields 'APV' 1 0.
          d_tratar_fields 'RJT' 0 0.
        ENDIF.

        PERFORM set_bloqueio  USING gw_ordens-aufnr 'D'.

      WHEN '0120'.
        READ TABLE gt_saida_0120 INTO gw_saida_0120 INDEX e_row.

        IF ( gw_saida_0120-rowcolor EQ c_red
        AND  gt_estrategia IS NOT INITIAL ).
          CLEAR gt_fields.
          d_tratar_fields 'APV' 1 0.
          d_tratar_fields 'RJT' 0 0.
        ENDIF.

        gw_ordens-aufnr = gw_saida_0120-aufnr.
        gw_ordens-user4 = gw_saida_0120-vlr_estimado.
        gw_ordens-erdat = gw_saida_0120-dt_solicitacao.
        gw_ordens-equnr = |{ gw_saida_0120-equipment ALPHA = OUT }| .
        gw_ordens-eqktx = gw_saida_0120-equipment_desc.
        gw_ordens-ktext = gw_saida_0120-short_text.
    ENDCASE.

    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '=ENT'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_hotspot_click.
    DATA: wl_ordem TYPE ty_ordens.

    CASE g_ts_0100-subscreen.
      WHEN '0110'.

        READ TABLE gt_ordens INTO wl_ordem INDEX e_row_id.
        FREE gt_bdcdata.

        CASE e_column_id.
          WHEN 'AUFNR'.
            SET PARAMETER ID 'ANR' FIELD wl_ordem-aufnr.

            IF ( wl_ordem-rowcolor = c_red ).
              CALL TRANSACTION 'IW32' AND SKIP FIRST SCREEN.
            ELSE.
              CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
            ENDIF.

          WHEN 'ICON'.

            CHECK wl_ordem-icon IS NOT INITIAL.

            PERFORM catsxt_simple USING wl_ordem-aufnr 1.

        ENDCASE.

      WHEN '0120'.

        READ TABLE gt_saida_0120 INTO gw_saida_0120 INDEX e_row_id.
        FREE gt_bdcdata.

        CASE e_column_id.
          WHEN 'AUFNR'.
            SET PARAMETER ID 'ANR' FIELD gw_saida_0120-aufnr.
            CALL TRANSACTION 'IW33' AND SKIP FIRST SCREEN.
          WHEN 'ICON'.

            CHECK gw_saida_0120-icon IS NOT INITIAL.

            PERFORM catsxt_simple USING gw_saida_0120-aufnr 1.

          WHEN 'ICON1'.

            CHECK gw_saida_0120-icon1 IS NOT INITIAL.

            PERFORM catsxt_simple USING gw_saida_0120-aufnr 2.
        ENDCASE.

    ENDCASE.

  ENDMETHOD.                    "ON_HOTSPOT_CLICK

  METHOD on_data_changed.

    DATA lw_good TYPE lvc_s_modi.

    DATA: tl_texto           TYPE catsxt_longtext_itab,
          vl_observacao(500).

    LOOP AT er_data_changed->mt_good_cells INTO lw_good.
      CHECK ( lw_good-fieldname = 'VLR_ESTIMADO' ).
      READ TABLE gt_saida_0120 INTO gw_saida_0120 INDEX lw_good-row_id.

      gw_saida_0120-vlr_estimado = lw_good-value.

      CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
        EXPORTING
          im_title = 'Observação'
        CHANGING
          ch_text  = tl_texto.

      IF sy-ucomm NE 'CX_CONT'.
        FREE: tl_texto.
      ENDIF.

      LOOP AT tl_texto INTO DATA(wa).
        vl_observacao = |{ vl_observacao } { wa }|.
      ENDLOOP.

      DATA(tamanho_string) = strlen( vl_observacao ).

      IF tamanho_string >= 200.
        MESSAGE |Quantidade de caracteres { tamanho_string } Ultrapassa o Valor Permitido "200"!| TYPE 'I'.
        EXIT.
      ENDIF.

      MODIFY gt_saida_0120 FROM gw_saida_0120 INDEX lw_good-row_id
      TRANSPORTING vlr_estimado.

      UPDATE zpmr0006
         SET vlr_estimado = gw_saida_0120-vlr_estimado
             observacao   = vl_observacao
             status       = c_pendente
       WHERE aufnr  EQ gw_saida_0120-aufnr
         AND status EQ c_rejeitado.

      COMMIT WORK.

      CALL METHOD obj_grid_0120->refresh_table_display
        EXPORTING
          is_stable = gw_stable.
    ENDLOOP.

    CLEAR er_data_changed->mt_good_cells.
  ENDMETHOD.                    "ON_DATA_CHANGED


ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


START-OF-SELECTION.

  PERFORM:
  f_selecionar_estrategia,
  f_selecionar_ordens,
  f_selecionar_suplementos,
  f_selecionar_perm_solic.

END-OF-SELECTION.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*



MODULE user_command_0100 INPUT.

  DATA: ls_ordem TYPE zpm_ordem_orc.

  CASE sy-ucomm.
    WHEN 'EXIT' OR 'CANCEL' OR 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'REFRESH'.
      CLEAR gw_ordens.

      PERFORM:
      f_selecionar_estrategia,
      f_selecionar_ordens,
      f_selecionar_suplementos,
      f_selecionar_perm_solic.


*      PERFORM F_SELECIONAR_ORDENS.
*      PERFORM F_SELECIONAR_SUPLEMENTOS.

      LEAVE TO CURRENT TRANSACTION.

    WHEN 'SOLIC_SUPL'.
      CLEAR: gw_solic_suplem,
             gt_editor.


      MOVE sy-uname TO gw_solic_suplem-user.
      CALL SCREEN 0200 STARTING AT 5 5.

    WHEN 'APR_COLET'.
      DATA: t_selected_rows TYPE lvc_t_row.
      CALL METHOD obj_grid_ordem->get_selected_rows
        IMPORTING
          et_index_rows = t_selected_rows.

      DATA: return      TYPE bapiret2,
            lv_id       TYPE belnr_d,
            lt_zpmr0002 TYPE TABLE OF zpmr0002.

      DATA(lt_ordens_aux) = gt_ordens.

      LOOP AT t_selected_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
        READ TABLE lt_ordens_aux ASSIGNING <fs_ordens> INDEX <fs_rows>-index.
        IF sy-subrc IS INITIAL.

          CLEAR: lv_id,
           ls_ordem-prox_aprov,
           return,
           ls_ordem.

*          PERFORM F_SELECIONAR_ORDENS.
          CALL FUNCTION 'ZPM_APROVAR_PERMIT'
            EXPORTING
              i_aufnr      = <fs_ordens>-aufnr
            IMPORTING
              return       = return
              id_orcamento = lv_id
              e_prox_aprov = ls_ordem-prox_aprov.

          IF  return-type = 'S'.

            IF lv_id IS NOT INITIAL.

              ls_ordem-id           = lv_id.
              ls_ordem-aufnr        = <fs_ordens>-aufnr.
              ls_ordem-istat        = 'L'.
              ls_ordem-ktext        = <fs_ordens>-ktext.
              ls_ordem-equnr        = <fs_ordens>-equnr.
              ls_ordem-erdat        = <fs_ordens>-erdat.
              ls_ordem-user4        = <fs_ordens>-user4.

            ELSE.

              ls_ordem-aufnr        = <fs_ordens>-aufnr.
              ls_ordem-ktext        = <fs_ordens>-ktext.
              ls_ordem-equnr        = <fs_ordens>-equnr.
              ls_ordem-erdat        = <fs_ordens>-erdat.
              ls_ordem-user4        = <fs_ordens>-user4.

              DELETE lt_zpmr0002 INDEX 1.

              LOOP AT lt_zpmr0002 ASSIGNING FIELD-SYMBOL(<fs_zpmr0002>).
                IF ls_ordem-prox_aprov IS INITIAL.
                  ls_ordem-prox_aprov = <fs_zpmr0002>-aprovador.
                ELSE.
                  ls_ordem-prox_aprov = ls_ordem-prox_aprov && ',' && <fs_zpmr0002>-aprovador.
                ENDIF.

              ENDLOOP.

            ENDIF.


            TRY .
                zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
              CATCH zcx_integracao INTO DATA(zcx_integracao).
                MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
                 NUMBER zcx_integracao->zif_error~msgno
                   WITH zcx_integracao->zif_error~msgv1
                        zcx_integracao->zif_error~msgv2
                        zcx_integracao->zif_error~msgv3
                        zcx_integracao->zif_error~msgv4.
              CATCH zcx_error INTO DATA(zcx_error).
                MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
                 NUMBER zcx_error->zif_error~msgno
                   WITH zcx_error->zif_error~msgv1
                        zcx_error->zif_error~msgv2
                        zcx_error->zif_error~msgv3
                        zcx_error->zif_error~msgv4.

            ENDTRY.

          ENDIF.

          IF return IS NOT INITIAL.
            MESSAGE return-message TYPE return-type.
          ENDIF.

          DELETE gt_ordens WHERE aufnr = <fs_ordens>-aufnr.

*          CLEAR gw_ordens.

*          CALL METHOD OBJ_GRID_ORDEM->REFRESH_TABLE_DISPLAY.

        ENDIF.
      ENDLOOP.
      CALL METHOD obj_grid_ordem->refresh_table_display
        EXPORTING
          is_stable = gw_grid_ordem.

      LEAVE TO CURRENT TRANSACTION.

    WHEN 'LIBERAR'.

      CASE g_ts_0100-subscreen.
        WHEN '0110'.
          IF ( gw_ordens IS INITIAL ).
            MESSAGE 'De um duplo click na coluna centro p/ selecionar!' TYPE 'S' DISPLAY LIKE 'E'.
*            MESSAGE 'Selecionar uma ordem p/ liberar!' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          "Verificando se o valor estimado é menor que o valor planejado para a liberação da ordem. AOENNING.

          IF gw_ordens-wert2 IS NOT INITIAL.
            IF gw_ordens-wert2 > gw_ordens-user4.
              MESSAGE ' Valor planejado é maior que o valor estimado ' TYPE 'I' DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.
          ENDIF.

*          DATA: return      TYPE bapiret2,
*                lv_id       TYPE belnr_d,
*                lt_zpmr0002 TYPE TABLE OF zpmr0002.

*          PERFORM F_SELECIONAR_ORDENS.
          CALL FUNCTION 'ZPM_APROVAR_PERMIT'
            EXPORTING
              i_aufnr      = gw_ordens-aufnr
            IMPORTING
              return       = return
              id_orcamento = lv_id
              e_prox_aprov = ls_ordem-prox_aprov.

          IF  return-type = 'S'.

            IF lv_id IS NOT INITIAL.

              ls_ordem-id           = lv_id.
              ls_ordem-aufnr        = gw_ordens-aufnr.
              ls_ordem-istat        = 'L'.
              ls_ordem-ktext        = gw_ordens-ktext.
              ls_ordem-equnr        = gw_ordens-equnr.
              ls_ordem-erdat        = gw_ordens-erdat.
              ls_ordem-user4        = gw_ordens-user4.

            ELSE.

              ls_ordem-aufnr        = gw_ordens-aufnr.
              ls_ordem-ktext        = gw_ordens-ktext.
              ls_ordem-equnr        = gw_ordens-equnr.
              ls_ordem-erdat        = gw_ordens-erdat.
              ls_ordem-user4        = gw_ordens-user4.

              DELETE lt_zpmr0002 INDEX 1.

              LOOP AT lt_zpmr0002 ASSIGNING <fs_zpmr0002>.
                IF ls_ordem-prox_aprov IS INITIAL.
                  ls_ordem-prox_aprov = <fs_zpmr0002>-aprovador.
                ELSE.
                  ls_ordem-prox_aprov = ls_ordem-prox_aprov && ',' && <fs_zpmr0002>-aprovador.
                ENDIF.

              ENDLOOP.

            ENDIF.


            TRY .
                zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
              CATCH zcx_integracao INTO zcx_integracao.
                MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
                 NUMBER zcx_integracao->zif_error~msgno
                   WITH zcx_integracao->zif_error~msgv1
                        zcx_integracao->zif_error~msgv2
                        zcx_integracao->zif_error~msgv3
                        zcx_integracao->zif_error~msgv4.
              CATCH zcx_error INTO zcx_error.
                MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
                 NUMBER zcx_error->zif_error~msgno
                   WITH zcx_error->zif_error~msgv1
                        zcx_error->zif_error~msgv2
                        zcx_error->zif_error~msgv3
                        zcx_error->zif_error~msgv4.

            ENDTRY.

          ENDIF.

          IF return IS NOT INITIAL.
            MESSAGE return-message TYPE return-type.
          ENDIF.

          DELETE gt_ordens WHERE aufnr = gw_ordens-aufnr.

          CLEAR gw_ordens.

*          CALL METHOD OBJ_GRID_ORDEM->REFRESH_TABLE_DISPLAY.

          CALL METHOD obj_grid_ordem->refresh_table_display
            EXPORTING
              is_stable = gw_grid_ordem.

          LEAVE TO CURRENT TRANSACTION.

        WHEN '0120'.

          DATA lt_return TYPE TABLE OF bapiret2.

          IF ( gw_ordens IS INITIAL ).
            MESSAGE 'De um duplo click na coluna centro p/ selecionar!' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ZPM_APROVAR_SUPLEMENTACAO'
            EXPORTING
              ordem        = gw_ordens-aufnr
              usuario      = sy-uname
            IMPORTING
              e_prox_aprov = ls_ordem-prox_aprov
              id_orcamento = lv_id
            TABLES
              errors       = lt_return.

          IF  lt_return IS INITIAL.

            IF lv_id IS NOT INITIAL.

              ls_ordem-id           = lv_id.
              ls_ordem-aufnr        = gw_ordens-aufnr.
              ls_ordem-istat        = 'L'.
              ls_ordem-ktext        = gw_ordens-ktext.
              ls_ordem-equnr        = gw_ordens-equnr.
              ls_ordem-erdat        = gw_ordens-erdat.
              ls_ordem-vlr_estimado = gw_ordens-user4.

            ELSE.

              ls_ordem-aufnr        = gw_ordens-aufnr.
              ls_ordem-ktext        = gw_ordens-ktext.
              ls_ordem-equnr        = gw_ordens-equnr.
              ls_ordem-erdat        = gw_ordens-erdat.
              ls_ordem-vlr_estimado = gw_ordens-user4.

            ENDIF.


            TRY .
                zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
              CATCH zcx_integracao INTO zcx_integracao.
                MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
                 NUMBER zcx_integracao->zif_error~msgno
                   WITH zcx_integracao->zif_error~msgv1
                        zcx_integracao->zif_error~msgv2
                        zcx_integracao->zif_error~msgv3
                        zcx_integracao->zif_error~msgv4.
              CATCH zcx_error INTO zcx_error.
                MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
                 NUMBER zcx_error->zif_error~msgno
                   WITH zcx_error->zif_error~msgv1
                        zcx_error->zif_error~msgv2
                        zcx_error->zif_error~msgv3
                        zcx_error->zif_error~msgv4.

            ENDTRY.

          ENDIF.

          IF lt_return IS INITIAL.
            MESSAGE 'Suplementação liberada!' TYPE 'S'.
            DELETE gt_saida_0120 WHERE aufnr = gw_ordens-aufnr.
          ELSE.
            CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
              EXPORTING
                it_message = lt_return.
          ENDIF.

          CLEAR gw_ordens.

          CALL METHOD obj_grid_0120->refresh_table_display
            EXPORTING
              is_stable = gw_grid_supl.

          LEAVE TO CURRENT TRANSACTION.

      ENDCASE.

*** Stefanini - IR244107 - 11/07/2025 - FINC - Início de Alteração
      INCLUDE zcoupa_integra_ordens IF FOUND.
*** Stefanini - IR244107 - 11/07/2025 - FINC - Fim de Alteração
    WHEN 'REJEITAR'.

      CASE g_ts_0100-subscreen.
        WHEN '0110'.
          IF ( gw_ordens IS INITIAL ).
            MESSAGE 'Selecionar uma ordem p/ rejeitar!' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ZPM_REPROVAR_PERMIT'
            EXPORTING
              ordem      = gw_ordens-aufnr
*** Stefanini - IR209426 - 18/11/2024 - LAZAROSR - Início de Alteração
              usuario    = sy-uname
*** Stefanini - IR209426 - 18/11/2024 - LAZAROSR - Fim de Alteração
            IMPORTING
              observacao = ls_ordem-observacao.

          ls_ordem-aufnr        = gw_ordens-aufnr.
          ls_ordem-ktext        = gw_ordens-ktext.
          ls_ordem-equnr        = gw_ordens-equnr.
          ls_ordem-erdat        = gw_ordens-erdat.
          ls_ordem-user4        = gw_ordens-user4.
          ls_ordem-istat        = 'R'.

          TRY .
              zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
            CATCH zcx_integracao INTO zcx_integracao.
              MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
               NUMBER zcx_integracao->zif_error~msgno
                 WITH zcx_integracao->zif_error~msgv1
                      zcx_integracao->zif_error~msgv2
                      zcx_integracao->zif_error~msgv3
                      zcx_integracao->zif_error~msgv4.
            CATCH zcx_error INTO zcx_error.
              MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
               NUMBER zcx_error->zif_error~msgno
                 WITH zcx_error->zif_error~msgv1
                      zcx_error->zif_error~msgv2
                      zcx_error->zif_error~msgv3
                      zcx_error->zif_error~msgv4.

          ENDTRY.

          CHECK sy-ucomm EQ 'CX_CONT'.

          gw_ordens-rowcolor = c_red.
          MODIFY gt_ordens FROM gw_ordens TRANSPORTING rowcolor
           WHERE aufnr = gw_ordens-aufnr.

          MESSAGE 'Ordem rejeitada!' TYPE 'S'.
          DELETE gt_ordens WHERE aufnr = gw_ordens-aufnr.
          CLEAR gw_ordens.

          CALL METHOD obj_grid_ordem->refresh_table_display.

          LEAVE TO CURRENT TRANSACTION.

        WHEN '0120'.
          IF ( gw_ordens IS INITIAL ).
            MESSAGE 'Selecionar um suplemento p/ rejeitar!' TYPE 'S' DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          CALL FUNCTION 'ZPM_REPROVAR_SUPLEMENTACAO'
            EXPORTING
              ordem      = gw_ordens-aufnr
            IMPORTING
              observacao = ls_ordem-observacao.

          ls_ordem-aufnr        = gw_ordens-aufnr.
          ls_ordem-ktext        = gw_ordens-ktext.
          ls_ordem-equnr        = gw_ordens-equnr.
          ls_ordem-erdat        = gw_ordens-erdat.
          ls_ordem-user4        = gw_ordens-user4.
          ls_ordem-istat        = 'R'.

          TRY .
              zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem ).
            CATCH zcx_integracao INTO zcx_integracao.
              MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
               NUMBER zcx_integracao->zif_error~msgno
                 WITH zcx_integracao->zif_error~msgv1
                      zcx_integracao->zif_error~msgv2
                      zcx_integracao->zif_error~msgv3
                      zcx_integracao->zif_error~msgv4.
            CATCH zcx_error INTO zcx_error.
              MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
               NUMBER zcx_error->zif_error~msgno
                 WITH zcx_error->zif_error~msgv1
                      zcx_error->zif_error~msgv2
                      zcx_error->zif_error~msgv3
                      zcx_error->zif_error~msgv4.

          ENDTRY.

          gw_saida_0120-rowcolor = c_red.
          MODIFY gt_saida_0120 FROM gw_saida_0120 TRANSPORTING rowcolor
           WHERE aufnr = gw_ordens-aufnr.

          COMMIT WORK.
          MESSAGE 'Suplementação rejeitada!' TYPE 'S'.
          DELETE gt_saida_0120 WHERE aufnr = gw_ordens-aufnr.
          CLEAR gw_ordens.
          CALL METHOD obj_grid_0120->refresh_table_display.

      ENDCASE.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS '0100' EXCLUDING gt_ucomm.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_ORDENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_ordens .
  DATA: lv_lines TYPE numc2,
        vlres    TYPE p DECIMALS 2,
        vlreq    TYPE p DECIMALS 2.

  DATA: esheader  TYPE bapi_alm_order_header_e,
        it_return TYPE TABLE OF bapiret2,
        it_olist  TYPE TABLE OF bapi_alm_order_objectlist,
        isheader  TYPE TABLE OF bapi_alm_order_headers_i.


  FREE gt_ordens.

  CALL FUNCTION 'ZPM_LISTAR_PERMITS1'
    EXPORTING
      i_user   = sy-uname
    IMPORTING
      e_lines  = lv_lines
    TABLES
      t_ordens = gt_ordens_aux.

  MOVE-CORRESPONDING gt_ordens_aux TO gt_ordens.
  IF gt_ordens IS NOT INITIAL.

    DATA(lt_ordens) = gt_ordens.
    SORT lt_ordens BY aufnr werks.
    DELETE ADJACENT DUPLICATES FROM lt_ordens COMPARING aufnr werks.

    SELECT *
      FROM afko AS a
      INNER JOIN resb AS b ON b~rsnum = a~rsnum
      INTO CORRESPONDING FIELDS OF TABLE it_resb
      FOR ALL ENTRIES IN lt_ordens
      WHERE a~aufnr EQ lt_ordens-aufnr
        AND b~werks EQ lt_ordens-werks.
    IF sy-subrc IS INITIAL.
      SORT it_resb BY aufnr werks.
      DATA(lt_resb) = it_resb.

      SORT lt_resb BY matnr werks.
      DELETE ADJACENT DUPLICATES FROM lt_resb COMPARING matnr werks.

      SELECT *
        INTO TABLE @DATA(lt_mbew)
        FROM mbew
        FOR ALL ENTRIES IN @lt_resb
        WHERE matnr EQ @lt_resb-matnr
          AND bwkey EQ @lt_resb-werks.
      IF sy-subrc IS INITIAL.
        SORT lt_mbew BY matnr bwkey.
      ENDIF.
    ENDIF.


    SELECT *
      FROM aufk
      INTO TABLE @DATA(lt_aufk)
      FOR ALL ENTRIES IN @lt_ordens
      WHERE aufnr EQ @lt_ordens-aufnr
        AND werks EQ @lt_ordens-werks.
    IF sy-subrc IS INITIAL.
      SORT lt_aufk BY aufnr werks.
    ENDIF.

    lt_ordens = gt_ordens.
    SORT lt_ordens BY aufnr.
    DELETE ADJACENT DUPLICATES FROM lt_ordens COMPARING aufnr.

    SELECT *
    FROM afko AS a
    INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
    INTO CORRESPONDING FIELDS OF TABLE it_afvc
      FOR ALL ENTRIES IN lt_ordens
    WHERE a~aufnr = lt_ordens-aufnr.
    IF sy-subrc IS INITIAL.
      SORT it_afvc BY aufnr.

      DATA(lt_afvc) = it_afvc.
      SORT lt_afvc BY rshty rshid.
      DELETE ADJACENT DUPLICATES FROM lt_afvc COMPARING rshty rshid.

      SELECT *
        FROM crhd
        INTO TABLE @DATA(lt_crhd)
        FOR ALL ENTRIES IN @lt_afvc
        WHERE objty = @lt_afvc-rshty
          AND objid = @lt_afvc-rshid.
      IF sy-subrc IS INITIAL.
        SORT lt_crhd BY objty objid.
      ENDIF.

      lt_afvc = it_afvc.
      SORT lt_afvc BY tplnr.
      DELETE ADJACENT DUPLICATES FROM lt_afvc COMPARING tplnr.

      SELECT *
        FROM iflotx
        INTO TABLE @DATA(lt_iflotx)
        FOR ALL ENTRIES IN @lt_afvc
        WHERE tplnr = @lt_afvc-tplnr
          AND spras = @sy-langu.
      IF sy-subrc IS INITIAL.
        SORT lt_iflotx BY tplnr.
      ENDIF.
    ENDIF.

    "FF #185560 - inicio
    SELECT a~aufnr,
           b~iloan,
           c~tplnr,
           d~pltxt
      FROM aufk AS a
      INNER JOIN afih  AS b ON b~aufnr = a~aufnr
      LEFT JOIN iloa  AS c ON c~iloan = b~iloan
      LEFT JOIN iflotx AS d ON d~tplnr = c~tplnr
                           AND d~spras = @sy-langu
      INTO TABLE @DATA(lt_ordens_locinst)
      FOR ALL ENTRIES IN @lt_ordens
      WHERE a~aufnr = @lt_ordens-aufnr.
    "FF #185560 - fim


    LOOP AT gt_ordens ASSIGNING FIELD-SYMBOL(<ls_ordens>).

*      SELECT *
*      FROM afko AS a
*      INNER JOIN resb AS b ON b~rsnum = a~rsnum
*      INTO CORRESPONDING FIELDS OF TABLE it_resb
*      WHERE a~aufnr EQ <ls_ordens>-aufnr
*        AND b~werks EQ <ls_ordens>-werks.


      READ TABLE lt_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>)
      WITH KEY aufnr = <ls_ordens>-aufnr
               werks = <ls_ordens>-werks
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*-> reread order status text in print language
        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            flg_user_stat    = abap_true
            objnr            = <fs_aufk>-objnr
            only_active      = abap_true
            spras            = sy-langu
          IMPORTING
            line             = <ls_ordens>-sttxt
            user_line        = <ls_ordens>-asttx
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.

        IF <ls_ordens>-asttx IS NOT INITIAL.

          "Seleção tipo de ordem.
          SELECT SINGLE a~* FROM t003o AS a
          INNER JOIN aufk AS b ON b~auart EQ a~auart
          INTO @DATA(wa_t003o)
           WHERE aufnr EQ @<ls_ordens>-aufnr.
          IF sy-subrc EQ 0.
            SELECT SINGLE * FROM  tj30t
            INTO @DATA(w_tj30t)
            WHERE  stsma       = @wa_t003o-stsma "Ajuste seleção status conforme cada tipo de ordem / IR230935.
            AND    txt04       = @<ls_ordens>-asttx+0(4)
            AND    spras       = @sy-langu.

            IF w_tj30t IS NOT INITIAL.
              <ls_ordens>-asttx = |{ <ls_ordens>-asttx+0(4) } - { w_tj30t-txt30 }| .
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.




      IF it_resb IS NOT INITIAL.
        READ TABLE it_resb TRANSPORTING NO FIELDS
        WITH KEY aufnr = <ls_ordens>-aufnr
                 werks = <ls_ordens>-werks
        BINARY SEARCH.
*        LOOP AT it_resb ASSIGNING FIELD-SYMBOL(<ls_resb>) WHERE aufnr EQ <ls_ordens>-aufnr.
        LOOP AT it_resb ASSIGNING FIELD-SYMBOL(<ls_resb>) FROM sy-tabix.
          IF <ls_resb>-aufnr <> <ls_ordens>-aufnr OR
             <ls_resb>-werks <> <ls_ordens>-werks.
            EXIT.
          ENDIF.

          READ TABLE lt_mbew ASSIGNING FIELD-SYMBOL(<fs_mbew>)
          WITH KEY matnr = <ls_resb>-matnr
                   bwkey = <ls_resb>-werks
          BINARY SEARCH.

          IF sy-subrc = 0.
            <ls_resb>-vprsv = <fs_mbew>-vprsv.

            IF <ls_resb>-vprsv = 'V'.
              <ls_resb>-preis = <fs_mbew>-stprs.
            ELSE.
              <ls_resb>-preis = <fs_mbew>-verpr.
            ENDIF.

            <ls_resb>-p_valor = ( <ls_resb>-bdmng * <ls_resb>-preis ).
          ENDIF.

          ADD <ls_resb>-p_valor TO <ls_ordens>-wert2.
        ENDLOOP.
      ENDIF.

*      SELECT *
*      FROM afvc AS a
*      INNER JOIN afko AS b ON b~aufpl EQ a~aufpl AND b~aufnr EQ <ls_ordens>-aufnr
*      INTO CORRESPONDING FIELDS OF TABLE it_afvc
*      WHERE a~aufnr

*      SELECT *
*      FROM afko AS a
*      INNER JOIN afvc AS b ON b~aufpl EQ a~aufpl
*      INTO CORRESPONDING FIELDS OF TABLE it_afvc
*      WHERE a~aufnr = <ls_ordens>-aufnr.

      READ TABLE it_afvc TRANSPORTING NO FIELDS
      WITH KEY aufnr = <ls_ordens>-aufnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
*      LOOP AT it_afvc ASSIGNING FIELD-SYMBOL(<ls_afvc>) WHERE aufnr EQ <ls_ordens>-aufnr.
        LOOP AT it_afvc ASSIGNING FIELD-SYMBOL(<ls_afvc>) FROM sy-tabix.

          IF <ls_afvc>-aufnr <> <ls_ordens>-aufnr.
            EXIT.
          ENDIF.

          ADD <ls_afvc>-preis TO <ls_ordens>-wert2.

        ENDLOOP.
        "FF #185560 - inicio
*        READ TABLE lt_iflotx ASSIGNING FIELD-SYMBOL(<fs_iflotx>)
*        WITH KEY tplnr = <ls_afvc>-tplnr
*        BINARY SEARCH.
*        IF sy-subrc IS INITIAL.
*          <ls_ordens>-pltxt = <fs_iflotx>-pltxt.
*        ENDIF.
        "FF #185560 - fim

      ENDIF.


      IF <ls_ordens>-user4 < <ls_ordens>-wert2.
        CLEAR wa_color.
        MOVE 'WERT2'    TO wa_color-fname.
        MOVE '6'        TO wa_color-color-col.
        MOVE '1'        TO wa_color-color-int.
        MOVE '1'        TO wa_color-color-inv.
        APPEND wa_color TO it_color.
        <ls_ordens>-cell_color[] = it_color.
      ENDIF.

*      "Inicio USER STORY 75882 - Anderson Oenning - 20/05/2022
      "Selecionar dados local / Centro de trabalho.

      CLEAR: esheader.
      FREE: it_return, it_olist.
      "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
*      CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
*        EXPORTING                              "#EC CI_USAGE_OK[2669857] ->  lista de obj não foi utilizada.
*          number    = <ls_ordens>-aufnr
*        IMPORTING
*          es_header = esheader
*        TABLES
*          return    = it_return
*          et_olist  = it_olist
*        EXCEPTIONS
*          OTHERS    = 01.
      READ TABLE it_afvc ASSIGNING <ls_afvc>
      WITH KEY aufnr = <ls_ordens>-aufnr
      BINARY SEARCH.
      IF sy-subrc EQ 0.
*        <ls_ordens>-tplnr = <ls_afvc>-tplnr. "FF #185560
        READ TABLE lt_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>)
        WITH KEY objty = <ls_afvc>-rshty
                 objid = <ls_afvc>-rshid
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <ls_ordens>-arbpl = <fs_crhd>-arbpl.
        ENDIF.

      ENDIF.

*      "Fim USER STORY 75882

      "FF #185560 - inicio
      READ TABLE lt_ordens_locinst WITH KEY aufnr = <ls_ordens>-aufnr INTO DATA(wa_locins).
      IF sy-subrc = 0.

        <ls_ordens>-tplnr = wa_locins-tplnr.
        <ls_ordens>-pltxt = wa_locins-pltxt.

      ENDIF.

      <ls_ordens>-equnr = |{ <ls_ordens>-equnr ALPHA = OUT }  |.

      "FF #185560 - fim



    ENDLOOP.

    LOOP AT gt_ordens ASSIGNING <fs_ordens>.
      IF ( <fs_ordens>-status = 'R' ).
        <fs_ordens>-rowcolor = c_red.
        <fs_ordens>-icon = '@DH@'.
      ELSE.
        <fs_ordens>-rowcolor = abap_false.
        <fs_ordens>-icon = abap_false.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "FF #185560 - inicio

  "=== Ocultar ordens rejeitadas (status = 'R') se o usuário não estiver na ZPMR0007 === Permissões p/ solicitação de suplemento PM
  SELECT SINGLE usnam
    FROM zpmr0007
    WHERE usnam = @sy-uname
    INTO @DATA(lv_user_found).

  IF sy-subrc <> 0.
    DELETE gt_ordens WHERE status = 'R'.
  ENDIF.

  "FF #185560 - fim


ENDFORM.                    " F_SELECIONAR_ORDENS

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_SOLICITACOES_SUPLEMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_selecionar_suplementos.

  DATA: lv_lines       TYPE numc2,
        lv_aufnr       TYPE aufnr,
        lv_valor_aprov TYPE dmbtr,
        lo_util        TYPE REF TO zcl_util.

  DATA: esheader  TYPE bapi_alm_order_header_e,
        it_return TYPE TABLE OF bapiret2,
        it_olist  TYPE TABLE OF bapi_alm_order_objectlist,
        isheader  TYPE TABLE OF bapi_alm_order_headers_i.

  CLEAR: gt_zpmr0006,
         gt_saida_0120,
         gt_fieldname.

  CALL FUNCTION 'ZPM_LISTAR_SUPLEMENTOS1'
    EXPORTING
      username      = sy-uname
    TABLES
      t_suplementos = gt_zpmr0006.

  CREATE OBJECT lo_util.

  LOOP AT gt_zpmr0006 INTO DATA(_zpmr0006).
    MOVE-CORRESPONDING _zpmr0006 TO gw_saida_0120.

    IF ( gw_saida_0120-status EQ 'R' ).
      gw_saida_0120-rowcolor = c_red.

      d_field_style_edit 'VLR_ESTIMADO' abap_true.
      gw_cellcolor-fname     = 'VLR_ESTIMADO'.
      gw_cellcolor-color-col = col_negative.
      gw_cellcolor-color-inv = '1'.

      APPEND gw_cellcolor TO gw_saida_0120-cellcolor.

    ELSE.
      d_field_style_edit 'VLR_ESTIMADO' space.
      gw_saida_0120-rowcolor = space.
    ENDIF.

    gw_saida_0120-icon1 = '@DH@'.

    IF gw_saida_0120-obs_reprov IS NOT INITIAL.
      gw_saida_0120-icon = '@DH@'.
    ELSE.
      gw_saida_0120-icon = abap_false.
    ENDIF.

    lo_util->field_style_edit( EXPORTING i_fieldnames = gt_fieldname
                               CHANGING  e_style      = gw_saida_0120-style ).

    SELECT SINGLE *
       FROM aufk
       INTO @DATA(w_aufk)
       WHERE aufnr EQ @_zpmr0006-aufnr
         AND werks EQ @_zpmr0006-werks.

*-> reread order status text in print language
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        flg_user_stat    = abap_true
        objnr            = w_aufk-objnr
        only_active      = abap_true
        spras            = sy-langu
      IMPORTING
        line             = gw_saida_0120-sttxt
        user_line        = gw_saida_0120-asttx
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    "Seleção tipo de ordem.
    SELECT SINGLE a~* FROM t003o AS a
    INNER JOIN aufk AS b ON b~auart EQ a~auart
    INTO @DATA(wa_t003o)
     WHERE aufnr EQ @gw_saida_0120-aufnr.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM  tj30t
      INTO @DATA(w_tj30t)
      WHERE  stsma       = @wa_t003o-stsma "Ajuste seleção status conforme cada tipo de ordem / IR230935.
      AND    txt04       = @gw_saida_0120-asttx+0(4)
      AND    spras       = @sy-langu.

      IF w_tj30t IS NOT INITIAL.
        gw_saida_0120-asttx = |{ gw_saida_0120-asttx+0(4) } - { w_tj30t-txt30 }| .
      ENDIF.
    ENDIF.

*    if gw_saida_0120-asttx is not initial.
*      select single * from  tj30t into @data(w_tj30t)
*             where  stsma       = 'ZPM00010'
*             and    txt04       = @gw_saida_0120-asttx
*             and    spras       = @sy-langu.
*
*      if w_tj30t is not initial.
*        gw_saida_0120-asttx = |{ gw_saida_0120-asttx } - { w_tj30t-txt30 }| .
*      endif.
*    endif.

*      "Inicio USER STORY 75882 - Anderson Oenning - 20/05/2022
    "Selecionar dados local / Centro de trabalho.

    CLEAR: esheader.
    FREE: it_return, it_olist.
    "*---> 05/07/2023 - Migração S4 - LO --> Material não foi utilizado
    CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING                              "#EC CI_USAGE_OK[2669857] ->  lista de obj não foi utilizada.
        number    = _zpmr0006-aufnr
      IMPORTING
        es_header = esheader
      TABLES
        return    = it_return
        et_olist  = it_olist
      EXCEPTIONS
        OTHERS    = 01.

    IF sy-subrc EQ 0.
      gw_saida_0120-tplnr = esheader-funct_loc.
      gw_saida_0120-arbpl = esheader-mn_wk_ctr.
    ENDIF.


*      "Fim USER STORY 75882

    APPEND gw_saida_0120 TO gt_saida_0120.
  ENDLOOP.


  "FF #185560 - inicio
  "=== Ocultar ordens rejeitadas (status = 'R') se o usuário não estiver na ZPMR0007 === Permissões p/ solicitação de suplemento PM
  SELECT SINGLE usnam
    FROM zpmr0007
    WHERE usnam = @sy-uname
    INTO @DATA(lv_user_found).

  IF sy-subrc <> 0.
    DELETE gt_saida_0120 WHERE status = 'R'.
  ENDIF.
  "FF #185560 - fim



ENDFORM.                    "SELECIONA_SOLICITACOES_SUPLEMENTO

*&---------------------------------------------------------------------*
*&      Form  F_PERMISSOES_SOLICITAR
*&---------------------------------------------------------------------*
FORM f_selecionar_perm_solic.
  REFRESH gt_ucomm.

  SELECT SINGLE *
    FROM zpmr0007
    INTO gw_zpmr0007
   WHERE usnam = sy-uname.

  CHECK ( sy-subrc IS NOT INITIAL ).

  "Desabilita botão para solicitar suplemento;
  APPEND 'SOLIC_SUPL' TO gt_ucomm.
ENDFORM.                    "F_PERMISSOES_SOLICITAR

*&---------------------------------------------------------------------*
*&      Module  MO_CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE mo_criar_objetos OUTPUT.
*----------------------------------------------------------*
* ORDENS PENDENTES P/ APROV *
*----------------------------------------------------------*

  IF ( obj_cont_ordem IS INITIAL ).
    gw_layout-zebra      = abap_true.
*    gw_layout-no_rowmark = 'X'.

    CREATE OBJECT obj_cont_ordem
      EXPORTING
        container_name = 'OBJ_CONT_ORD'.

    CREATE OBJECT obj_grid_ordem
      EXPORTING
        i_parent = obj_cont_ordem.

    gt_function = VALUE #(
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_undo )
                            ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_check )
                            ( cl_gui_alv_grid=>mc_fc_refresh )
                         ).

    PERFORM f_montar_fieldcat USING c_ordem.

    gw_layout-info_fname = 'ROWCOLOR'.  "Row color
    gw_layout-ctab_fname = 'CELL_COLOR'.
    gw_layout-col_opt = 'X'.                                "FF #185560
*    gw_layout-edit = abap_true.

    LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs>).       "FF #185560
      <fs>-col_opt = 'X'.
    ENDLOOP..

    CALL METHOD obj_grid_ordem->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
        is_layout            = gw_layout
        it_toolbar_excluding = gt_function
      CHANGING
        it_fieldcatalog      = gt_fieldcat[]
        it_outtab            = gt_ordens[].


    CALL METHOD obj_grid_ordem->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    SET HANDLER: lcl_event_handler=>on_double_click FOR obj_grid_ordem,
                 lcl_event_handler=>on_hotspot_click FOR obj_grid_ordem.
  ELSE.
    CALL METHOD obj_grid_ordem->refresh_table_display.
  ENDIF.

*----------------------------------------------------------*
* ESTRATÉGIA DE APROVADORES                                *
*----------------------------------------------------------*

  IF ( obj_cont_estrategia IS INITIAL ).
    gw_layout-zebra      = abap_true.

    CREATE OBJECT obj_cont_estrategia
      EXPORTING
        container_name = 'OBJ_CONT_EST'.

    CREATE OBJECT obj_grid_estrategia
      EXPORTING
        i_parent = obj_cont_estrategia.

    gt_function = VALUE #(
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_undo )
                            ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_check )
                            ( cl_gui_alv_grid=>mc_fc_refresh )
                            ( cl_gui_alv_grid=>mc_fc_select_all )
                            ( cl_gui_alv_grid=>mc_fc_sort_asc )
                            ( cl_gui_alv_grid=>mc_fc_sort_dsc )
                            ( cl_gui_alv_grid=>mc_fc_subtot )
                            ( cl_gui_alv_grid=>mc_fc_sum )
                            ( cl_gui_alv_grid=>mc_mb_export )
                            ( cl_gui_alv_grid=>mc_mb_filter )
                            ( cl_gui_alv_grid=>mc_mb_paste )
                            ( cl_gui_alv_grid=>mc_mb_subtot )
                            ( cl_gui_alv_grid=>mc_fc_print )
                            ( cl_gui_alv_grid=>mc_mb_sum )
                         ).

    PERFORM f_montar_fieldcat USING c_estrategia.

    CALL METHOD obj_grid_estrategia->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = gt_function
        is_layout            = gw_layout
      CHANGING
        it_fieldcatalog      = gt_fieldcat[]
        it_outtab            = gt_estrategia[].

  ELSE.
    CALL METHOD obj_grid_estrategia->refresh_table_display.
  ENDIF.

*--------------------------------------------------*
* TELA DE SUPLEMENTAÇÕES PENDENTES                 *
*--------------------------------------------------*
  FREE gt_function.

  IF ( obj_cont_0120 IS INITIAL ).

    gw_layout-zebra      = abap_true.
    gw_variant-report    = sy-repid.
    gw_layout-info_fname = 'ROWCOLOR'.
    gw_layout-ctab_fname = 'CELLCOLOR'.
    gw_layout-stylefname = 'STYLE'.
    gw_layout-no_rowmark = 'X'.
    gw_stable-row = 'X'.
    gw_stable-col = 'X'.

    CREATE OBJECT obj_cont_0120
      EXPORTING
        container_name = 'CUSTOM_0120'.

    CREATE OBJECT obj_grid_0120
      EXPORTING
        i_parent = obj_cont_0120.

    gt_function =  VALUE #(
                            ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_move_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste )
                            ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_undo )
                            ( cl_gui_alv_grid=>mc_fc_loc_append_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy )
                            ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_loc_cut )
                            ( cl_gui_alv_grid=>mc_fc_check )
                            ( cl_gui_alv_grid=>mc_fc_refresh )
                           ).

    PERFORM f_montar_fieldcat USING c_suplementos.

    CALL METHOD obj_grid_0120->set_table_for_first_display
      EXPORTING
        is_layout            = gw_layout
        it_toolbar_excluding = gt_function
        is_variant           = gw_variant
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = gt_fieldcat[]
        it_outtab            = gt_saida_0120.

    SET HANDLER:
    lcl_event_handler=>on_double_click  FOR obj_grid_0120,
    lcl_event_handler=>on_hotspot_click FOR obj_grid_0120,
    lcl_event_handler=>on_data_changed  FOR obj_grid_0120.

    CALL METHOD obj_grid_0120->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_grid_0120->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD obj_grid_0120->refresh_table_display.
  ENDIF.

ENDMODULE.                 " MO_CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0260   text
*----------------------------------------------------------------------*
FORM f_montar_fieldcat USING p_alv.
  FREE gt_fieldcat.

  "FF #185560 - inicio
  "===Determina se usuário é aprovador (ZPMR0002 ou ZPMR0011) ===

  DATA(lv_aprovador) = abap_false.

  SELECT SINGLE * FROM zpmr0002
    WHERE aprovador  = @sy-uname
       OR usua_subst = @sy-uname
    INTO @DATA(ls_0002).

  IF sy-subrc = 0.
    lv_aprovador = abap_true.
  ENDIF.

  SELECT SINGLE * FROM zpmr0011
    WHERE aprovador  = @sy-uname
       OR usua_subst = @sy-uname
    INTO @DATA(ls_0011).

  IF sy-subrc = 0.
    lv_aprovador = abap_true.
  ENDIF.


  "=== Define flag de edição baseado no perfil ===
  IF lv_aprovador = abap_true.
    DATA(lv_edit_flag) = space. "bloqueia edição
  ELSE.
    lv_edit_flag = 'X'.   "permite edição
  ENDIF.

  "FF #185560 - fim

  CASE p_alv.
    WHEN c_ordem.
      d_preenche_fieldcat:
          'WERKS'       'CHAR'  'Centro'         ''  ' '  ' '  '06' ' ' ' ' ,
          'ERDAT'       'DATS'  'Data'           ''  ' '  ' '  '10' ' ' ' ' ,
          'AUFNR'       'CHAR'  'Ordem'          ''  'X'  ' '  '08' 'X' ' ',
          'USER4'       'CURR'  'Vlr Estimado'   ''  ' '  ' '  '13' ' ' ' ', "FF #185560
          'EQUNR'       'CHAR'  'Equipamento'    ''  ''   ' '  '20' ' ' ' ',
          'EQKTX'       'CHAR'  'Desc.Equip'     ''  ''   ' '  '50' ' ' ' ',
          'TPLNR'       'CHAR'  'Local.Inst'     ''  ''   ' '  '15' ' ' ' ',
          'PLTXT'       'CHAR'  'Desc.Local.Inst'     ''  ''   ' '  '50' ' ' ' ',
*          'ARBPL'       'CHAR'  'Ctr.Trab'       ''  ''   ' '  '15' ' ' ' ', "FF #185560
          'KTEXT'       'CHAR'  'Descrição'      ''  ' '  ' '  '37' ' ' ' ',
          'WERT2'       'CURR'  'Vlr Planejado'  ''  ' '  ' '  '13' ' ' ' ',
          'ASTTX'       'CHAR'  'Status do Usuario' ''  ' '  ' '  '15' ' ' ' ',
          'ICON'        '    '  'Observação  '   ''  ' '  ' '  '10' 'X' 'C'.



    WHEN c_estrategia.
      d_preenche_fieldcat:
          'CENTRO_DESP' 'CHAR'  'Centro'         ''  ' '  ' '  '06'  ' ' ' ',
          'APROVADOR'   'CHAR'  'Usuário'        ''  ' '  ' '  '12'  ' ' ' ',
          'VALOR_DE'    'CURR'  'Valor de'       ''  ' '  ' '  '15'  ' ' ' ',
          'VALOR_ATE'   'CURR'  'Valor até'      ''  ' '  ' '  '15'  ' ' ' '.

    WHEN c_suplementos.
      d_preenche_fieldcat:
          'WERKS'          ''      'Centro'            ' '  ' '  ' '  '06'  ' ' ' ',
          'DT_SOLICITACAO' ''      'Data'              ' '  ' '  ' '  '12'  ' ' ' ',
          'AUFNR'          ''      'Ordem'             ' '  'X'  ' '  '12'  'X' ' ',
*          'TPLNR'          ''      'Local.Inst'        ' '  ' '  ' '  '15'  ' ' ' ',
*          'ARBPL'          ''      'Ctr.Trab  '             ' '  ' '  ' '  '15'  ' ' ' ', "FF #185560

          'VLR_ESTIMADO'   'DMBTR' 'Valor'             ' '  ' '  lv_edit_flag  '12'  ' ' ' ', "FF #185560

          'SOLICITANTE'    ''      'Solicitante'       ' '  ' '  ' '  '15'  ' ' ' ',
          'ASTTX      '    ''      'Status do Usuario' ' '  ' '  ' '  '16'  ' ' ' ',
          'ICON1'          ''      'Obs Solicitação'   ' '  ' '  ' '  '7'   'X' 'C',
          'ICON'           ''      'Obs Rej'           ' '  ' '  ' '  '7'   'X' 'C'.
  ENDCASE.
ENDFORM.                    " F_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_ESTRATEGIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_estrategia .

  DATA data_atual TYPE datum.

  DATA: BEGIN OF tl_centro OCCURS 0,
          iwerk  TYPE equz-iwerk,
          centro TYPE c LENGTH 9,
        END OF tl_centro.

  REFRESH gt_fields.


  SELECT *
   INTO CORRESPONDING FIELDS OF TABLE gt_estrategia
   FROM zpmr0002
   WHERE aprovador EQ sy-uname OR usua_subst EQ sy-uname. "Anderson Oenning

  LOOP AT gt_estrategia ASSIGNING FIELD-SYMBOL(<wa_estrategia>).
    IF <wa_estrategia>-aprovador IS NOT INITIAL
      AND <wa_estrategia>-usua_subst IS NOT INITIAL
      AND <wa_estrategia>-data_lim >= sy-datum.
      <wa_estrategia>-aprovador = <wa_estrategia>-usua_subst.
    ENDIF.
  ENDLOOP.

  DELETE gt_estrategia WHERE aprovador NE sy-uname.

  IF ( sy-subrc IS NOT INITIAL ).
    d_tratar_fields 'APV' 0 1.
    d_tratar_fields 'RJT' 0 1.
  ENDIF.

  "FF #185560 - inicio
  SELECT * FROM zpmr0011
  WHERE aprovador  = @sy-uname OR
        usua_subst = @sy-uname
  INTO TABLE @gt_aprovadores_cc.
  "FF #185560 - fim


ENDFORM.                    " F_SELECIONAR_ESTRATEGIA
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: ls_ordem2     TYPE zpm_ordem_orc,
        lv_prox_aprov TYPE zpm_ordem_orc-prox_aprov,
        lv_objnr      TYPE ihsg-objnr,
        lv_valor      TYPE zpmr0002-valor_de.

  CASE sy-ucomm.
    WHEN 'EXIT'.
      CLEAR gw_solic_suplem.
      LEAVE TO SCREEN 0.

    WHEN 'ENTER' OR 'OK'.
      DATA: lv_aufnr TYPE aufnr.
      CLEAR: gw_aufk, gt_editor, gw_zpmr0007.

      lv_aufnr = |{ gw_solic_suplem-aufnr ALPHA = IN }|.

      CALL METHOD obj_custom_editor->get_text_as_stream
        IMPORTING
          text = gt_editor.

      SELECT SINGLE *
        FROM aufk
        INTO gw_aufk
       WHERE aufnr = lv_aufnr.

      IF ( sy-subrc IS INITIAL ).


        "//Tabela de permissões p/ solicitação de suplemento;
        SELECT SINGLE *
          FROM zpmr0007
          INTO gw_zpmr0007
         WHERE werks = gw_aufk-werks
           AND usnam = sy-uname.

        "//Verifica se possuí permissão p/ o centro da ordem;
        IF ( gw_zpmr0007-werks NE gw_aufk-werks ).
          MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.

        ENDIF.


        "_____________________________________
        "Verificar se existe orçamento inicial aprovado para ordem.
        zcl_ordem_man=>m_check_orc_inic_ord(
          EXPORTING
            i_aufnr  = lv_aufnr    " Nº ordem
          IMPORTING
            i_retorn = DATA(i_retorn)  " Campo de texto do comprimento 1
            i_belnr  = DATA(i_belnr)   " Valor total em moeda de transação
        ).


        IF i_belnr IS INITIAL.
          MESSAGE TEXT-009 TYPE 'I' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.


*** > Início JAP - Abaco 16.05.2017
*        IF ( GW_AUFK-AUART NE 'ZPM7' AND GW_AUFK-AUART NE 'ID07').
*          MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*** US #170302 - MMSILVA - 24.06.2025 - Ini ***

        " Inicio Comentário - MMSILVA - 24.06.2025 - US #170302
*        select zval into table it_zval
*          from ztparam
*          where param eq c_tp_ordem
*            and const eq gw_zpmr0007-werks.
*        read table it_zval with key zval = gw_aufk-auart.
*        if not sy-subrc is initial.
*          message text-003 type 'S' display like 'E'.
        " Fim Comentário - MMSILVA - 24.06.2025 - US #170302

        SELECT * FROM zpmt0064
          INTO TABLE @DATA(it_zpmt0064)
          WHERE centro EQ @gw_aufk-werks
          AND   tipo   EQ @gw_aufk-auart.

        IF NOT sy-subrc IS INITIAL.
          MESSAGE TEXT-003 TYPE 'S' DISPLAY LIKE 'E'.
*** US #170302 - MMSILVA - 24.06.2025 - Fim ***
*** < Fim  JAP - Abaco 16.05.2017
        ELSEIF ( gw_aufk-phas1 NE 'X' ).
          MESSAGE TEXT-004 TYPE 'S' DISPLAY LIKE 'E'.

        ELSEIF ( gt_editor IS INITIAL ).
          MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.

        ELSE.
          SELECT SINGLE *
            FROM zpmr0006
            INTO gw_zpmr0006
           WHERE aufnr  EQ lv_aufnr
             AND status NE c_liberado.

          IF ( sy-subrc IS NOT INITIAL ).
            DATA user_data    TYPE alm_me_user_data.
            DATA order_header TYPE alm_me_order_header.
            DATA user_profile TYPE alm_me_c010prf.

            CLEAR gw_zpmr0006.

            "//Obter texto do editor;
            LOOP AT gt_editor INTO gw_editor.
              CONCATENATE gw_zpmr0006-observacao gw_editor INTO gw_zpmr0006-observacao.
            ENDLOOP.

            CALL FUNCTION 'ALM_ME_ORDER_GETDETAIL'
              EXPORTING
                orderid       = lv_aufnr
                resource      = 'X'
                userdata      = user_data
                order_profile = user_profile
              IMPORTING
                order_header  = order_header
              EXCEPTIONS
                read_error    = 1.

            gw_zpmr0006-status         = 'P'.                   "(P)- Pendente
            gw_zpmr0006-aufnr          = lv_aufnr.              "Ordem
            gw_zpmr0006-solicitante    = gw_solic_suplem-user.  "Requerente
            gw_zpmr0006-vlr_estimado   = gw_solic_suplem-valor. "Custo global estimado da ordem
            gw_zpmr0006-dt_solicitacao = sy-datum.              "Data da solicitação
            gw_zpmr0006-werks          = gw_aufk-werks.         "Centro
            gw_zpmr0006-equipment      = order_header-equipment.
            gw_zpmr0006-equipment_desc = order_header-equipment_desc.
            gw_zpmr0006-short_text     = order_header-short_text.
            gw_zpmr0006-object         = order_header-object_no.
            gw_zpmr0006-currency       = order_header-currency.

            INSERT zpmr0006 FROM gw_zpmr0006.
            CLEAR gw_zpmr0006.

            COMMIT WORK.

            lv_valor = order_header-estimated_costs + gw_solic_suplem-valor.

            "FF #185560 - inicio
*            SELECT *
*              FROM zpmr0002
*              INTO TABLE @DATA(lt_zpmt0002)
*              WHERE centro_desp = @order_header-plant
*                AND valor_de   <= @lv_valor
*                AND nivel       = '0000000001'.
*            IF sy-subrc IS INITIAL.
*              SORT lt_zpmt0002 BY nivel.
*
*              LOOP AT lt_zpmt0002 ASSIGNING FIELD-SYMBOL(<fs_aprov>).
*
*                IF sy-tabix = 1.
*
*                  IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
*                    CONCATENATE <fs_aprov>-usua_subst ',' INTO lv_prox_aprov.
*                  ELSE.
*                    CONCATENATE <fs_aprov>-aprovador ',' INTO lv_prox_aprov.
*                  ENDIF.
*
*                ELSE.
*
*                  IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
*                    CONCATENATE <fs_aprov>-usua_subst ',' lv_prox_aprov INTO lv_prox_aprov.
*                  ELSE.
*                    CONCATENATE <fs_aprov>-aprovador ',' lv_prox_aprov INTO lv_prox_aprov.
*                  ENDIF.
*
*                ENDIF.
*
*              ENDLOOP.

            "=== Primeiro, tenta achar aprovador prioritário por centro de custo na ZPMR0011 ===
            SELECT *
              FROM zpmr0011
              INTO TABLE @DATA(lt_zpmr0011)
              WHERE bukrs       = @order_header-comp_code
                AND centro_desp = @order_header-plant
                AND kostl       = @order_header-costcenter
                AND valor_de   <= @lv_valor
                AND valor_ate  >= @lv_valor
                AND nivel       = '0000000001'.

            READ TABLE lt_zpmr0011 ASSIGNING FIELD-SYMBOL(<fs_0011>) INDEX 1.

            IF sy-subrc = 0.
              "=== Achou aprovador por centro de custo (prioridade) ===
              IF <fs_0011>-usua_subst IS NOT INITIAL AND <fs_0011>-data_lim >= sy-datum.
                CONCATENATE <fs_0011>-usua_subst ',' INTO lv_prox_aprov.
              ELSE.
                CONCATENATE <fs_0011>-aprovador ',' INTO lv_prox_aprov.
              ENDIF.

            ELSE.
              "=== Não achou na 0011 → segue regra da 0002 ===
              SELECT *
                FROM zpmr0002
                INTO TABLE @DATA(lt_zpmt0002)
                WHERE centro_desp = @order_header-plant
                  AND valor_de   <= @lv_valor
                  AND nivel       = '0000000001'.

              IF sy-subrc IS INITIAL.
                SORT lt_zpmt0002 BY nivel.

                LOOP AT lt_zpmt0002 ASSIGNING FIELD-SYMBOL(<fs_aprov>).

                  IF sy-tabix = 1.
                    IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
                      CONCATENATE <fs_aprov>-usua_subst ',' INTO lv_prox_aprov.
                    ELSE.
                      CONCATENATE <fs_aprov>-aprovador ',' INTO lv_prox_aprov.
                    ENDIF.
                  ELSE.
                    IF <fs_aprov>-usua_subst IS NOT INITIAL AND <fs_aprov>-data_lim >= sy-datum.
                      CONCATENATE <fs_aprov>-usua_subst ',' lv_prox_aprov INTO lv_prox_aprov.
                    ELSE.
                      CONCATENATE <fs_aprov>-aprovador ',' lv_prox_aprov INTO lv_prox_aprov.
                    ENDIF.
                  ENDIF.
                ENDLOOP.
              ENDIF.


              "FF #185560 - fim


              ls_ordem2-aufnr        = lv_aufnr.
              ls_ordem2-ktext        = order_header-short_text.
              ls_ordem2-equnr        = order_header-equipment.
              ls_ordem2-erdat        = sy-datum.
              ls_ordem2-vlr_estimado = gw_solic_suplem-valor.
              ls_ordem2-prox_aprov   = lv_prox_aprov.

              TRY .
                  zcl_int_ob_ordem_orc_mobman=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = ls_ordem2 ).
                CATCH zcx_integracao INTO zcx_integracao.
                  MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
                   NUMBER zcx_integracao->zif_error~msgno
                     WITH zcx_integracao->zif_error~msgv1
                          zcx_integracao->zif_error~msgv2
                          zcx_integracao->zif_error~msgv3
                          zcx_integracao->zif_error~msgv4.
                CATCH zcx_error INTO zcx_error.
                  MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
                   NUMBER zcx_error->zif_error~msgno
                     WITH zcx_error->zif_error~msgv1
                          zcx_error->zif_error~msgv2
                          zcx_error->zif_error~msgv3
                          zcx_error->zif_error~msgv4.

              ENDTRY.

            ENDIF.


            MESSAGE TEXT-006 TYPE 'I' DISPLAY LIKE 'S'.
            LEAVE TO SCREEN 0.

          ELSE.
            MESSAGE TEXT-007 TYPE 'I' DISPLAY LIKE 'W'.
          ENDIF.
        ENDIF.

      ELSE.
        MESSAGE TEXT-008 TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.


    WHEN OTHERS.

  ENDCASE.



ENDMODULE.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TS_SET_ACTIVE_TAB  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

MODULE ts_set_active_tab OUTPUT.
  ts_0100-activetab = g_ts_0100-pressed_tab.

  CASE g_ts_0100-pressed_tab.
    WHEN c_ts_100-tab1.
      g_ts_0100-subscreen = '0110'.

      btn_libera    = '@01@ Liberar Ordem'.
      btn_apr_colet = '@01@ Aprovação Coletiva'.
      btn_rejeitar  = '@02@ Rejeitar Ordem'.

    WHEN c_ts_100-tab2.
      g_ts_0100-subscreen = '0120'.

      btn_libera   = '@01@ Liberar Suplemento'.
      btn_apr_colet = '@01@ Aprovação Coletiva'.
      btn_rejeitar = '@02@ Rejeitar Suplemento'.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.                 " TS_SET_ACTIVE_TAB  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TS_GET_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ts_get_active_tab INPUT.
  CASE sy-ucomm.
    WHEN c_ts_100-tab1.
      CLEAR gw_ordens.", GT_FIELDS.
      g_ts_0100-pressed_tab = c_ts_100-tab1.

*      D_TRATAR_FIELDS 'RJT' 1 0.

    WHEN c_ts_100-tab2.
      CLEAR gw_ordens.
      g_ts_0100-pressed_tab = c_ts_100-tab2.

*      D_TRATAR_FIELDS 'RJT' 1 0.

    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TS_GET_ACTIVE_TAB  INPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  IF ( obj_custom_txt IS INITIAL ).

    CREATE OBJECT obj_custom_txt
      EXPORTING
        container_name = 'CUSTOM_TEXTEDIT'.

    CREATE OBJECT obj_custom_editor
      EXPORTING
        parent            = obj_custom_txt
        wordwrap_mode     = 1
        wordwrap_position = 76
        max_number_chars  = 200.

    CALL METHOD obj_custom_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.
  ENDIF.

  CALL METHOD obj_custom_editor->set_text_as_stream
    EXPORTING
      text = gt_editor.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TRATAR_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_fields OUTPUT.

  LOOP AT gt_fields INTO gw_fields.
    LOOP AT SCREEN.
      CHECK screen-group1 EQ gw_fields-group1.
      screen-input     = gw_fields-value.
      screen-invisible = gw_fields-invisible.
      MODIFY SCREEN.

    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATAR_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CHECK_BLOQUEIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_ORDENS_AUFNR  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM check_bloqueio USING p_aufnr CHANGING p_subrc.

  DATA: tl_enq     TYPE TABLE OF seqg3 WITH HEADER LINE,
        wl_num_enq TYPE sy-tabix,
        wl_arg     TYPE seqg3-garg,
        aufnr      TYPE seqg3-garg.

  CLEAR p_subrc.

  wl_arg = |{ sy-mandt }{ p_aufnr }|.
  aufnr = |{ p_aufnr ALPHA = OUT }|.
  CONDENSE aufnr NO-GAPS.

  CALL FUNCTION 'ENQUEUE_READ'
    EXPORTING
      gname  = 'AUFK'
      guname = '*'
      garg   = wl_arg
    IMPORTING
      number = wl_num_enq
    TABLES
      enq    = tl_enq.

  IF wl_num_enq NE 0.
    CLEAR gw_ordens.
    p_subrc = 4.
    READ TABLE tl_enq INDEX 1.
    MESSAGE |Ordem { aufnr } Bloqueada por { tl_enq-guname }.| TYPE 'E'.
  ENDIF.

ENDFORM.

FORM set_bloqueio USING p_aufnr dir.

  DATA: funcao TYPE string.

  CASE dir.
    WHEN 'D'.
      funcao = 'CO_ZF_ORDER_DELOCK'.
    WHEN 'L'.
      funcao = 'CO_ZF_ORDER_LOCK'.
  ENDCASE.

  CALL FUNCTION funcao
    EXPORTING
      aufnr  = p_aufnr
      client = sy-mandt.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CATSXT_SIMPLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_ORDEM_AUFNR  text
*----------------------------------------------------------------------*
FORM catsxt_simple USING p_ordem dir.

  DATA: cont     TYPE i,
        tl_texto TYPE TABLE OF txline,
        obs      TYPE string.

  IF dir EQ 1.
    SELECT SINGLE obs_reprov
      FROM zpmr0006
      INTO @obs
      WHERE aufnr EQ @p_ordem.
  ELSE.

    SELECT SINGLE observacao
      FROM zpmr0006
      INTO @obs
      WHERE aufnr EQ @p_ordem.
  ENDIF.

  CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
    EXPORTING
      i_string         = obs
      i_tabline_length = 72
    TABLES
      et_table         = tl_texto.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Observação'
      im_display_mode = abap_true
    CHANGING
      ch_text         = tl_texto.


ENDFORM.
