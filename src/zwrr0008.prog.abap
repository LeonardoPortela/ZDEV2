*&---------------------------------------------------------------------*
*& Report  ZWRR0008
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwrr0008 MESSAGE-ID zfi.
INCLUDE <icon>.
TABLES: zib_nfe_dist_ter, zfiwrt0008, zfiwrt0001, lfa1.

CLASS lcl_event_receiver DEFINITION DEFERRED.

TYPES: BEGIN OF ty_saida,
         bukrs            TYPE zib_nfe_dist_ter-bukrs,
         branch           TYPE zib_nfe_dist_ter-branch,
         numero           TYPE zib_nfe_dist_ter-numero,
         serie            TYPE zib_nfe_dist_ter-serie,
         prod_qtd_comerci TYPE zib_nfe_dist_itm-prod_qtd_comerci,
         prod_vlr_und_com TYPE zib_nfe_dist_itm-prod_vlr_und_com,
         prod_vlr_total_b TYPE zib_nfe_dist_itm-prod_vlr_total_b,
         prod_pedido_comp TYPE zib_nfe_dist_itm-prod_pedido_comp,
         dt_emissao       TYPE zib_nfe_dist_ter-dt_emissao,
         prod_cfop        TYPE zib_nfe_dist_itm-prod_cfop,
         forne_cnpj       TYPE lfa1-stcd1,
         forne_cpf        TYPE lfa1-stcd2,
         forne_ie         TYPE zib_nfe_dist_ter-forne_ie,
         lifnr            TYPE lfa1-lifnr,
         name1            TYPE lfa1-name1,
         chave_nfe        TYPE zib_nfe_dist_ter-chave_nfe,
         seq_lcto         TYPE zfiwrt0008-seq_lcto,
         seq_lcto_out     TYPE zfiwrt0008-seq_lcto,
         destino_cnpj     TYPE lfa1-stcd1,
         destino_ie       TYPE zib_nfe_dist_ter-destino_ie,
         destino_lifnr    TYPE lfa1-lifnr,
         destino_name1    TYPE lfa1-name1,
         pbukrs           TYPE zib_nfe_dist_ter-bukrs,
         pbranch          TYPE zib_nfe_dist_ter-branch,
         bukrs_lcto       TYPE zfiwrt0008-bukrs,
         bldat_lcto       TYPE zfiwrt0008-bldat,
         budat_lcto       TYPE zfiwrt0008-budat,
         operacao         TYPE zfiwrt0001-operacao,
         f_transporte     TYPE zib_nfe_dist_ter-f_transporte,
         pc_partiner      TYPE zib_nfe_dist_ter-pc_partiner,
         lr_partiner      TYPE zib_nfe_dist_ter-lr_partiner,
         status           TYPE zfiwrs0004-status,
         status_proc      TYPE zfiwrs0004-status_proc,
         docnum_flag      TYPE zfiwrs0004-docnum_flag,

         doc_contab       TYPE zfiwrs0004-doc_contab,
         doc_contab_flag  TYPE zfiwrs0004-doc_contab_flag,

         belnr            TYPE zfiwrs0004-belnr,
         belnr_flag       TYPE zfiwrs0004-belnr_flag,

         vbeln_r          TYPE zfiwrs0004-vbeln_r,
         vbeln_flag       TYPE zfiwrs0004-vbeln_flag,

         doc_mat          TYPE zfiwrs0004-doc_mat,
         doc_mat_flag     TYPE zfiwrs0004-doc_mat_flag,

         doc_transf       TYPE zfiwrs0004-doc_transf,
         doc_transf_flag  TYPE zfiwrs0004-doc_transf_flag,

         docnum           TYPE zfiwrt0008-docnum,
         nivel_aprov      TYPE zfiwrs0004-nivel_aprov,
         aprov            TYPE zfiwrs0004-aprov,
         obj_key          TYPE zfiwrt0008-obj_key,

         ebeln            TYPE zfiwrt0008-ebeln,
         ebelp            TYPE zfiwrt0008-ebelp,

         loc_carrega      TYPE zfiwrt0025-loc_carrega,
         "c_operacao       TYPE zfiwrt0025-operacao,


         bloqueado,

         cancel           TYPE zib_nfe_dist_ter-cancel,
         partyp           TYPE j_1bad-partyp,

         cellstyles       TYPE lvc_t_styl,
         color_line(4)    TYPE c,
       END OF ty_saida.

TYPES: BEGIN OF ty_zib_nfe_dist_ter,
         bukrs        TYPE zib_nfe_dist_ter-bukrs,
         branch       TYPE zib_nfe_dist_ter-branch,
         numero       TYPE zib_nfe_dist_ter-numero,
         serie        TYPE zib_nfe_dist_ter-serie,
         dt_emissao   TYPE zib_nfe_dist_ter-dt_emissao,
         forne_ie     TYPE zib_nfe_dist_ter-forne_ie,
         forne_cnpj   TYPE lfa1-stcd1,
*-CS2021000595 - 22.06.2021 - JT - inicio
         forne_cpf    TYPE lfa1-stcd2,
*-CS2021000595 - 22.06.2021 - JT - fim
         chave_nfe    TYPE zib_nfe_dist_ter-chave_nfe,
         destino_cnpj TYPE lfa1-stcd1,
         destino_ie   TYPE zib_nfe_dist_ter-destino_ie,
         cancel       TYPE zib_nfe_dist_ter-cancel,
         f_transporte TYPE zib_nfe_dist_ter-f_transporte,
         pc_partiner  TYPE zib_nfe_dist_ter-pc_partiner,
         lr_partiner  TYPE zib_nfe_dist_ter-lr_partiner,
         bukrs_e      TYPE zib_nfe_dist_ter-bukrs_e,
         branch_e     TYPE zib_nfe_dist_ter-branch_e,
       END OF ty_zib_nfe_dist_ter.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

TYPES: BEGIN OF ty_setlinet,
         setname  TYPE setlinet-setname,
         lineid   TYPE setlinet-lineid,
         descript TYPE setlinet-descript,
         operacao TYPE zfiwrt0001-operacao.
TYPES: END OF ty_setlinet.

DATA: it_saida              TYPE TABLE OF  ty_saida,
      it_saida_mic          TYPE TABLE OF  ty_saida,
      wa_saida              TYPE ty_saida,
      wa_saida_mic          TYPE ty_saida,
      it_zib_nfe_dist_ter   TYPE TABLE OF ty_zib_nfe_dist_ter,
      wa_zib_nfe_dist_ter   TYPE ty_zib_nfe_dist_ter,
      it_zib_nfe_dist_itm   TYPE TABLE OF zib_nfe_dist_itm,
      wa_zib_nfe_dist_itm   TYPE zib_nfe_dist_itm,
      tg_1000               TYPE TABLE OF zfiwrt1000 WITH HEADER LINE,
      tg_set_dep_cfop_oper  TYPE TABLE OF setleaf WITH HEADER LINE,
      tg_set_item_cfop_oper TYPE TABLE OF ty_setlinet WITH HEADER LINE,
      tg_lfa1               TYPE TABLE OF lfa1,
      wg_lfa1               TYPE lfa1,
      tg_kna1               TYPE TABLE OF kna1,
      wg_kna1               TYPE kna1,
      it_lfa1               TYPE TABLE OF lfa1,
      wa_lfa1               TYPE lfa1,
      it_zfi08              TYPE TABLE OF zfiwrt0008,
      wa_zfi08              TYPE zfiwrt0008,
      it_zfi06              TYPE TABLE OF zfiwrt0006,
      it_zfiwrt0025         TYPE TABLE OF zfiwrt0025 WITH HEADER LINE,
      wa_zfi06              TYPE zfiwrt0006,
      wa_zfi0024            TYPE zfiwrt0024.

DATA: wl_saida_znfw0005 TYPE zfiwrs0004,
      it_saida_znfw0005 TYPE zfiwrs0004_t.

DATA: g_container     TYPE REF TO cl_gui_custom_container,
      g_grid          TYPE REF TO cl_gui_alv_grid,
      tl_function     TYPE ui_functions,
      wl_function     LIKE tl_function WITH HEADER LINE,
      gs_layout       TYPE lvc_s_layo,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      tg_selectedrow  TYPE lvc_t_row,
      wg_selectedrow  TYPE lvc_s_row.

DATA: event TYPE REF TO lcl_event_receiver.

DATA: it_fcat      TYPE TABLE OF lvc_s_fcat,
      wl_fcat      TYPE lvc_s_fcat,
      it_f4        TYPE lvc_t_f4 WITH HEADER LINE,
      wa_estrutura TYPE ty_estrutura,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_stable    TYPE lvc_s_stbl,
      wl_answer.

DATA: ob_timer     TYPE REF TO cl_gui_timer,
      wl_cont      TYPE sy-tabix,
      wg_ativo,
      l_stcd3_pesq TYPE stcd3,
      l_stcd3      TYPE stcd3,
      l_ie_num     TYPE p,
      tl_docs      TYPE TABLE OF zfiwrs0003,
      v_index      TYPE sy-tabix,
      l_perc       TYPE p DECIMALS 2.

DATA: fcode    TYPE TABLE OF sy-ucomm,
      wa_fcode TYPE sy-ucomm.

*&---------------------------------------------------------------------*
* Declaração de Constantes
*&---------------------------------------------------------------------*
*>C100: AZUL
*>C110: AZUL ESCURO
*>C200: CINZA
*>C210: CINZA ESCURO
*>C300: AMARELO
*>C310: AMARELA ESCURO
*>C400: AZUL
*>C410: AZUL ESCURO
*>C500: VERDE
*>C510: VERDE ESCURO
*>C510: VERMELHO
*>C510: VERMELHO ESCURO

CONSTANTS: BEGIN OF gc,
             inv      TYPE lvc_s_colo-inv      VALUE '0',
             int      TYPE lvc_s_colo-int      VALUE '1',
             padrao   TYPE lvc_s_colo-col      VALUE '2',
             amarelo  TYPE lvc_s_colo-col      VALUE '3',
             vermelho TYPE lvc_s_colo-col      VALUE '6',
           END OF gc.

CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    DATA: error_in_data  TYPE c.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
    METHODS handle_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
    METHODS perform_semantic_checks IMPORTING pr_data_changed TYPE REF TO cl_alv_changed_data_protocol.

ENDCLASS.

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD perform_semantic_checks.
    DATA: lv_value TYPE lvc_value.

    LOOP AT pr_data_changed->mt_good_cells INTO DATA(wa_good_cells).

      CASE wa_good_cells-fieldname.
        WHEN 'F_TRANSPORTE' OR 'PC_PARTINER' OR 'LR_PARTINER'.
          READ TABLE it_saida INDEX wa_good_cells-row_id INTO DATA(wa_saida).
          TRY .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro_cnpj_cpf_ie(
                 EXPORTING
                  i_cnpj          = CONV #( wa_saida-forne_cnpj )
                  i_insc_estatual = wa_saida-forne_ie
                )->ck_parceiro_local_negocio(
                 IMPORTING
                   e_j_1bbranch  = DATA(e_j_1bbranch)
                ).
            CATCH zcx_parceiros INTO DATA(ex_parceiros).
              MESSAGE s119(zfi) WITH wa_saida-forne_cnpj wa_saida-forne_ie.
              CALL METHOD pr_data_changed->add_protocol_entry
                EXPORTING
                  i_msgid     = sy-msgid
                  i_msgno     = sy-msgno
                  i_msgty     = 'E'
                  i_msgv1     = sy-msgv1
                  i_msgv2     = sy-msgv2
                  i_msgv3     = sy-msgv3
                  i_msgv4     = sy-msgv4
                  i_fieldname = wa_good_cells-fieldname
                  i_row_id    = wa_good_cells-row_id.
              CONTINUE.
          ENDTRY.
      ENDCASE.

      CASE wa_good_cells-fieldname.
        WHEN 'F_TRANSPORTE' OR 'PC_PARTINER'.

          IF wa_good_cells-value IS NOT INITIAL.
            TRY .
                zcl_fornecedores=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = CONV #( wa_good_cells-value )
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = e_j_1bbranch-bukrs
                  ).
              CATCH zcx_parceiros INTO ex_parceiros.
                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = ex_parceiros->zif_error~msgid
                    i_msgno     = ex_parceiros->zif_error~msgno
                    i_msgty     = 'E'
                    i_msgv1     = ex_parceiros->zif_error~msgv1
                    i_msgv2     = ex_parceiros->zif_error~msgv2
                    i_msgv3     = ex_parceiros->zif_error~msgv3
                    i_msgv4     = ex_parceiros->zif_error~msgv4
                    i_fieldname = wa_good_cells-fieldname
                    i_row_id    = wa_good_cells-row_id.
            ENDTRY.
          ENDIF.

        WHEN 'LR_PARTINER'.

          IF wa_good_cells-value IS NOT INITIAL.
            TRY .
                zcl_clientes=>zif_parceiros~get_instance(
                  )->set_parceiro( i_parceiro = CONV #( wa_good_cells-value )
                  )->ck_ativo(
                  )->ck_ativo_empresa( i_empresa = e_j_1bbranch-bukrs
                  ).
              CATCH zcx_parceiros INTO ex_parceiros.
                CALL METHOD pr_data_changed->add_protocol_entry
                  EXPORTING
                    i_msgid     = ex_parceiros->zif_error~msgid
                    i_msgno     = ex_parceiros->zif_error~msgno
                    i_msgty     = 'E'
                    i_msgv1     = ex_parceiros->zif_error~msgv1
                    i_msgv2     = ex_parceiros->zif_error~msgv2
                    i_msgv3     = ex_parceiros->zif_error~msgv3
                    i_msgv4     = ex_parceiros->zif_error~msgv4
                    i_fieldname = wa_good_cells-fieldname
                    i_row_id    = wa_good_cells-row_id.
            ENDTRY.
          ENDIF.

        WHEN 'LOC_CARREGA'.
          IF wa_good_cells-value IS NOT INITIAL.

            READ TABLE it_saida INDEX wa_good_cells-row_id INTO DATA(wa_saida_aux).

            SELECT *
            FROM zfiwrt0025 INTO TABLE it_zfiwrt0025
            WHERE bukrs EQ wa_saida_aux-bukrs
              AND loc_carrega EQ  wa_good_cells-value
              AND cfop EQ wa_saida_aux-prod_cfop.

            IF sy-subrc <> 0.
              MESSAGE 'Ação não permitida! Operação não parametrizada para o lançamento, favor cadastrar na Operações MIC !' TYPE 'I'.

              CLEAR: wa_saida_aux-operacao,
                     wa_saida_aux-destino_cnpj,
                     wa_saida_aux-destino_ie,
                     wa_saida_aux-destino_lifnr,
                     wa_saida_aux-destino_name1,
                     wa_saida_aux-f_transporte,
                     wa_saida_aux-lr_partiner,
                     wa_saida_aux-pc_partiner.

              MODIFY it_saida FROM wa_saida_aux INDEX wa_good_cells-row_id.
              g_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
              EXIT.
            ELSE.
              READ TABLE  it_zfiwrt0025 INTO DATA(wa_zfiwrt0025) WITH KEY  bukrs       = wa_saida_aux-bukrs
                                                                           loc_carrega = wa_good_cells-value
                                                                           cfop        = wa_saida_aux-prod_cfop.
              CLEAR: lv_value.
              MOVE wa_zfiwrt0025-operacao TO lv_value.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'OPERACAO'
                  i_value     = lv_value.

              CLEAR: lv_value.
              MOVE wa_good_cells-value TO lv_value.
              CALL METHOD pr_data_changed->modify_cell
                EXPORTING
                  i_row_id    = wa_good_cells-row_id
                  i_fieldname = 'LOC_CARREGA'
                  i_value     = lv_value.


              PERFORM  check_av_recebimento USING wa_zfiwrt0025-operacao
                                                  wa_saida_aux.

              MODIFY it_saida FROM wa_saida_aux INDEX wa_good_cells-row_id.

              CALL METHOD cl_gui_cfw=>flush.

              CALL METHOD g_grid->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.

            ENDIF.
          ENDIF.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD handle_data_changed.

    error_in_data = abap_false.
    CALL METHOD perform_semantic_checks( er_data_changed ).
    IF error_in_data = abap_true.
      CALL METHOD er_data_changed->display_protocol.
    ENDIF.

  ENDMETHOD.

  METHOD handle_data_changed_finished.

*    CHECK e_modified IS NOT INITIAL.
*
*    LOOP AT et_good_cells INTO DATA(wa_good_cells).
*      CASE wa_good_cells-fieldname.
*        WHEN 'F_TRANSPORTE' OR 'PC_PARTINER' OR 'LR_PARTINER'.
*          READ TABLE it_saida INDEX wa_good_cells-row_id INTO DATA(wa_saida).
*      ENDCASE.
*    ENDLOOP.
* g_grid->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).

  ENDMETHOD.
ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_nm  RADIOBUTTON GROUP g1 USER-COMMAND modifica_tela DEFAULT 'X',
              p_onf RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: bukrs    FOR zib_nfe_dist_ter-bukrs NO-EXTENSION NO INTERVALS, "OBLIGATORY
                  dt_emis  FOR zib_nfe_dist_ter-dt_emissao,
                  numero   FOR zib_nfe_dist_ter-numero,
                  seq_lcto FOR zfiwrt0008-seq_lcto.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3  WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: p_cnpj FOR lfa1-stcd1 MODIF ID t1, "OBLIGATORY,
*-CS2021000595 - 22.06.2021 - JT - inicio
                  p_cpf  FOR lfa1-stcd2 MODIF ID t1, "OBLIGATORY,
*-CS2021000595 - 22.06.2021 - JT - fim

                  p_chave    FOR zib_nfe_dist_ter-chave_nfe MODIF ID t1,

                  p_ope  FOR zfiwrt0001-operacao NO INTERVALS "OBLIGATORY
                                                 NO-EXTENSION MODIF ID t1 MATCHCODE OBJECT zfiwr_help_operacao.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4  WITH FRAME TITLE TEXT-003.
  PARAMETERS: pen RADIOBUTTON GROUP g2,
              ger RADIOBUTTON GROUP g2,
              am  RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b4.

INITIALIZATION.

*-CS2021000595 - 22.06.2021 - JT - inicio
*******************************************************************************************
* tratar empresa / cpf / cnpj
*******************************************************************************************
AT SELECTION-SCREEN.

  IF bukrs IS INITIAL and p_chave is INITIAL.
    MESSAGE s000(z01) WITH TEXT-100 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  IF p_onf = abap_true.
    IF p_ope IS INITIAL.
      MESSAGE s000(z01) WITH TEXT-101 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    IF ( p_cpf IS     INITIAL AND p_cnpj IS     INITIAL AND p_chave IS INITIAL ) OR
       ( p_cpf IS NOT INITIAL AND p_cnpj IS NOT INITIAL ).
      MESSAGE s000(z01) WITH TEXT-104 DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

*    IF p_ope-low = '0093'.
*      IF p_cpf IS INITIAL.
*        MESSAGE s000(z01) WITH text-102 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ELSE.
*      IF p_cnpj IS INITIAL.
*        MESSAGE s000(z01) WITH text-103 DISPLAY LIKE 'E'.
*        STOP.
*      ENDIF.
*    ENDIF.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
*******************************************************************************************

AT SELECTION-SCREEN OUTPUT.
  PERFORM modifica_tela.

START-OF-SELECTION.

  PERFORM: selecao_dados,
           alv.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotsopt_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no,
      on_f4 FOR EVENT onf4  OF cl_gui_alv_grid  IMPORTING e_fieldname
                                                          es_row_no
                                                          er_event_data
                                                          et_bad_cells
                                                          e_display,
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object e_interactive,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_hotsopt_click.

    DATA: opt            TYPE ctu_params,
          tl_1000        TYPE TABLE OF zfiwrt1000,
          vl_nfobjn      TYPE j_1binterf-nfobjn,
          vl_docnum      TYPE j_1bnfdoc-docnum,
          rspar_tab      TYPE TABLE OF rsparams,
          rspar_line     LIKE LINE OF rspar_tab,
          wl_obj_key     TYPE zib_contabil_chv-obj_key,
          wl_1000        TYPE zfiwrt1000,
          wl_zib_err_aux TYPE zib_contabil_err.

    DATA: it_zib_err TYPE TABLE OF zib_contabil_err.

    REFRESH: tl_1000.

    CASE e_column_id.
      WHEN 'SEQ_LCTO_OUT'.
        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ) AND ( wa_saida-seq_lcto IS NOT INITIAL ).

        SET PARAMETER ID 'SEQ' FIELD wa_saida-seq_lcto.
        CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

      WHEN 'DOCNUM_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        LOOP AT tg_1000 INTO tg_1000
          WHERE field    EQ 'DOCNUM'
            AND seq_lcto EQ wa_saida-seq_lcto.

          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.

        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSEIF wa_saida-docnum IS NOT INITIAL.

          vl_docnum = wa_saida-docnum.

          CALL FUNCTION 'J_1B_NF_DOC_READ_INTO_OBJECT'
            EXPORTING
              doc_number         = vl_docnum
            IMPORTING
              obj_number         = vl_nfobjn
            EXCEPTIONS
              document_not_found = 1
              docum_lock         = 2
              OTHERS             = 3.

          CALL FUNCTION 'J_1B_NF_OBJECT_DISPLAY'
            EXPORTING
              obj_number         = vl_nfobjn
            EXCEPTIONS
              object_not_found   = 1
              scr_ctrl_not_found = 2
              OTHERS             = 3.
        ENDIF.

      WHEN 'DOC_CONTAB_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        CLEAR: it_zib_err[].

        SELECT *
          FROM zib_contabil_err INTO TABLE it_zib_err
         WHERE obj_key EQ wa_saida-obj_key
            AND type   EQ 'E'.

        LOOP AT it_zib_err INTO DATA(wl_zib_err).
          MOVE: wl_zib_err-type    TO wl_1000-type,
                wl_zib_err-message TO wl_1000-mensagem.

          APPEND wl_1000 TO tl_1000.
          CLEAR: wl_1000.
        ENDLOOP.

        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSEIF wa_saida-doc_contab IS NOT INITIAL.

          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_contab.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs_lcto.
          SET PARAMETER ID 'GJR' FIELD wa_saida-budat_lcto(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'DOC_TRANSF_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        CLEAR: wl_obj_key.
        CONCATENATE wa_saida-obj_key 'I' INTO wl_obj_key.

        CLEAR: wl_zib_err_aux.
        SELECT SINGLE *
          INTO wl_zib_err_aux
          FROM zib_contabil_err
         WHERE obj_key = wl_obj_key
           AND type    = 'E'.

        IF sy-subrc = 0.
          MOVE: wl_zib_err_aux-type    TO wl_1000-type,
                wl_zib_err_aux-message TO wl_1000-mensagem.

          APPEND wl_1000 TO tl_1000.
          CLEAR: wl_1000.
        ENDIF.
        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSEIF wa_saida-doc_transf IS NOT INITIAL.
          SET PARAMETER ID 'BLN' FIELD wa_saida-doc_transf.
          SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs_lcto.
          SET PARAMETER ID 'GJR' FIELD wa_saida-budat_lcto(4).

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        ENDIF.

      WHEN 'VBELN_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        LOOP AT tg_1000 INTO tg_1000 WHERE field    EQ 'VBELN_R'
                                       AND seq_lcto EQ wa_saida-seq_lcto.
          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.

        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSE.

          SELECT SINGLE * INTO @DATA(wa_delivery)
            FROM likp
           WHERE vbeln EQ @wa_saida-vbeln_r.

          CASE wa_delivery-vbtyp.
            WHEN 'J'.
              SET PARAMETER ID 'VL'    FIELD wa_saida-vbeln_r.
              CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
            WHEN '7'.
              SET PARAMETER ID 'VL'  FIELD wa_saida-vbeln_r.
              SET PARAMETER ID 'VLM' FIELD wa_saida-vbeln_r.
              CALL TRANSACTION 'VL33N' AND SKIP FIRST SCREEN.
          ENDCASE.

        ENDIF.

      WHEN 'BELNR_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        LOOP AT tg_1000 INTO tg_1000 WHERE field    EQ 'IMOB'
                                       AND seq_lcto EQ wa_saida-seq_lcto.
          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.

        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSE.
          IF wa_saida-belnr IS NOT INITIAL..
            SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
            SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs_lcto.
            SET PARAMETER ID 'GJR' FIELD wa_saida-budat_lcto(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.

      WHEN 'DOC_MAT_FLAG'.

        READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.

        CHECK ( sy-subrc EQ 0 ).

        LOOP AT tg_1000 INTO tg_1000 WHERE field    EQ 'MBLNR'
                                       AND seq_lcto EQ wa_saida-seq_lcto.
          APPEND tg_1000 TO tl_1000.
          CLEAR: tg_1000.
        ENDLOOP.

        IF tl_1000[] IS NOT INITIAL.
          PERFORM chama_log_bapi TABLES tl_1000.
        ELSEIF wa_saida-doc_mat IS NOT INITIAL.

*---> 19.07.2023 19:08:04 - Migração S4 - DL
*          SET PARAMETER ID 'MBN' FIELD wa_saida-doc_mat.
*          SET PARAMETER ID 'GJR' FIELD wa_saida-budat_lcto(4).
*          CALL TRANSACTION 'MB03' AND SKIP FIRST SCREEN.

          CALL FUNCTION 'MIGO_DIALOG'
            EXPORTING
              i_action            = 'A04'
              i_refdoc            = 'R02'
              i_notree            = 'X'
              i_no_auth_check     = ' '
              i_deadend           = 'X'
              i_skip_first_screen = 'X'
              i_okcode            = 'OK_GO'
              i_mblnr             = wa_saida-doc_mat
              i_mjahr             = wa_saida-budat_lcto(4).
          "I_ZEILE = I_FINAL-ZEILE.
*<--- 19.07.2023 19:08:04 - Migração S4 - DL
        ENDIF.

      WHEN 'EBELN'.
        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
        SET PARAMETER ID 'BSP' FIELD wa_saida-ebelp.
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
    ENDCASE.

  ENDMETHOD.
  METHOD on_f4.

    DATA: lva_loc_carrega TYPE zfiwrt0025-loc_carrega.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.
    DATA: ls_modi TYPE lvc_s_modi.
    TYPES : BEGIN OF ty_lcarrega,
*              bukrs       TYPE zfiwrt0025-bukrs,
*              loc_carrega TYPE zfiwrt0025-loc_carrega,
*              cfop        TYPE zfiwrt0025-cfop,
*              operacao    TYPE zfiwrt0025-operacao,
              lifnr TYPE lfa1-lifnr,
              name1 TYPE lfa1-name1,
            END OF ty_lcarrega.

    DATA: tl_lcarrega   TYPE TABLE OF ty_lcarrega,
          wl_lcarrega   TYPE ty_lcarrega,
          tl_zfiwrt0025 TYPE TABLE OF zfiwrt0025,
          wl_zfiwrt0025 TYPE zfiwrt0025,
          tl_return_lg  TYPE TABLE OF ddshretval,
          tl_dselclg    TYPE TABLE OF dselc,
          wl_return_lg  TYPE  ddshretval.

    CASE e_fieldname.
      WHEN 'LOC_CARREGA'.
        SELECT lifnr name1
          FROM lfa1
        INTO TABLE tl_lcarrega.

        SORT tl_lcarrega BY lifnr.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'LIFNR'
            value_org       = 'S'
          TABLES
            value_tab       = tl_lcarrega
            return_tab      = tl_return_lg
            dynpfld_mapping = tl_dselclg.

        READ TABLE tl_return_lg INTO wl_return_lg INDEX 1.
        IF sy-subrc = 0 AND wl_return_lg-fieldval <> ''.

          lva_loc_carrega  = wl_return_lg-fieldval.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lva_loc_carrega
            IMPORTING
              output = lva_loc_carrega.

          SELECT *
            FROM zfiwrt0025
          INTO TABLE tl_zfiwrt0025
            WHERE loc_carrega EQ lva_loc_carrega
              AND bukrs EQ wa_saida-bukrs
              AND cfop EQ wa_saida-prod_cfop.

          IF tl_zfiwrt0025 IS NOT INITIAL.
            READ TABLE tl_zfiwrt0025 INTO wl_zfiwrt0025 INDEX 1.

            ASSIGN er_event_data->m_data->* TO <itab>.
            ls_modi-row_id    = es_row_no-row_id.
            ls_modi-fieldname = 'LOC_CARREGA'.
            ls_modi-value     = wl_zfiwrt0025-loc_carrega.
            APPEND ls_modi TO <itab>.

            ASSIGN er_event_data->m_data->* TO <itab>.
            ls_modi-row_id    = es_row_no-row_id.
            ls_modi-fieldname = 'OPERACAO'.
            ls_modi-value     = wl_zfiwrt0025-operacao.
            APPEND ls_modi TO <itab>.

            er_event_data->m_event_handled = 'X'.

          ELSE.
            MESSAGE 'Ação não permitida! Operação não parametrizada para o lançamento, favor cadastrar na Operações MIC !' TYPE 'S'.
            EXIT.
          ENDIF.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD handle_toolbar.
    DATA: ls_toolbar  TYPE stb_button.
    IF p_nm = 'X'.
      CLEAR ls_toolbar.
      MOVE 3 TO ls_toolbar-butn_type.
      APPEND ls_toolbar TO e_object->mt_toolbar.

* icone e novo botão.
      CLEAR ls_toolbar.
      MOVE 'MOVELINE'               TO ls_toolbar-function.
      MOVE icon_generate            TO ls_toolbar-icon.
      MOVE 'Copia Local Carregamento'(111) TO ls_toolbar-quickinfo.
      MOVE 'Cop. Loc. Carregamento'(112) TO ls_toolbar-text.
      MOVE ' ' TO ls_toolbar-disabled.
      APPEND ls_toolbar TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    DATA: lit_rows TYPE lvc_t_row.
    CASE e_ucomm.
      WHEN 'MOVELINE'.
        CALL METHOD g_grid->get_selected_rows
          IMPORTING
            et_index_rows = lit_rows.

        IF lit_rows IS INITIAL.
          MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
          EXIT.
        ELSE.
          PERFORM fm_move_values TABLES lit_rows.
        ENDIF.
        wa_stable = 'X'.
        CALL METHOD g_grid->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modifica_tela .
  LOOP AT SCREEN.
    READ TABLE p_ope INDEX 1.

    IF  p_nm = 'X'.
      IF screen-group1 = 'T1'.
        screen-invisible = 1.
        screen-input     = 0.
        screen-active    = 0.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ELSE.
      IF screen-group1 = 'T1'.
*-CS2021000595 - 22.06.2021 - JT - inicio
        IF screen-name CS 'P_CPF'.
*          IF p_ope-low = '0093'.
*            FREE: p_cnpj.
**           screen-invisible = 0.
*            screen-input     = 1.
**           screen-active    = 1.
*            MODIFY SCREEN.
*            CONTINUE.
*          ELSE.
*            FREE: p_cpf.
**           screen-invisible = 1.
*            screen-input     = 0.
**           screen-active    = 0.
*            MODIFY SCREEN.
*            CONTINUE.
*          ENDIF.
        ELSEIF screen-name CS 'P_CNPJ'.
*          IF p_ope-low = '0093'.
*            FREE: p_cnpj.
**           screen-invisible = 1.
*            screen-input     = 0.
**           screen-active    = 0.
*            MODIFY SCREEN.
*            CONTINUE.
*          ELSE.
*            FREE: p_cpf.
**           screen-invisible = 0.
*            screen-input     = 1.
*            screen-active    = 1.
*            MODIFY SCREEN.
*            CONTINUE.
*          ENDIF.
        ELSE.
          screen-invisible = 0.
          screen-input     = 1.
          screen-active    = 1.
          MODIFY SCREEN.
          CONTINUE.
        ENDIF.
      ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecao_dados.

  it_saida_mic[] = it_saida[].

  CLEAR: it_saida[], it_zib_nfe_dist_ter[], it_lfa1[], tg_lfa1[], it_zfi08[], it_saida_znfw0005[], tg_1000[], tg_set_dep_cfop_oper[], tg_set_item_cfop_oper[].

  AUTHORITY-CHECK OBJECT 'ZNFW_BUKRS'
    ID 'BUKRS' FIELD bukrs-low.  "Código Empresa

  IF sy-subrc EQ 0.

    IF p_nm IS NOT INITIAL.
      IF dt_emis IS INITIAL.
        MESSAGE 'Data Emissão Obrigatório!' TYPE 'S'.
        STOP.
      ELSE.
        PERFORM busca_dados_nota_mic.
      ENDIF.
    ELSE.
      IF dt_emis IS INITIAL and p_chave is INITIAL.
        MESSAGE 'Data Emissão Obrigatório!' TYPE 'S'.
        STOP.
*     ELSEIF p_cnpj IS INITIAL.
*       MESSAGE 'CNPJ Obrigatório!' TYPE 'S'.
*       STOP.
      ELSEIF p_ope IS INITIAL.
        MESSAGE 'Cód.Operação Obrigatório!' TYPE 'S'.
        STOP.
      ELSE.
        PERFORM busca_dados_outras_nf.
      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE 'Sem autorização para a empresa selecionada!' TYPE 'S'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

*** CS2020001418 ZNFW0009 - Inicio
  CLEAR: fcode , wa_fcode.
  IF p_onf IS NOT INITIAL.
    wa_fcode = 'CAD_OPMIC'. APPEND wa_fcode TO fcode.
  ENDIF.
*** CS2020001418 ZNFW0009 - Fim

  SET PF-STATUS 'ST_0100' EXCLUDING fcode.
  SET TITLEBAR 'TL_0100'.

  IF g_container IS INITIAL.

    wa_stable-row        = 'X'.
    wa_stable-col        = 'X'.
    gs_layout-stylefname = 'CELLSTYLES'.
    gs_layout-info_fname = 'COLOR_LINE'.

    CREATE OBJECT g_container
      EXPORTING
        container_name = 'CONTAINER'.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_container.

    CREATE OBJECT ob_timer
      EXPORTING
        parent = g_container.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    CALL METHOD cl_gui_cfw=>flush.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_outtab            = it_saida
        it_fieldcatalog      = it_fcat.

    REFRESH it_f4.
    it_f4-fieldname = 'LOC_CARREGA'.
    it_f4-register = 'X'.
    it_f4-getbefore = 'X'.
    it_f4-chngeafter ='X'.
    APPEND it_f4.

    CALL METHOD g_grid->register_f4_for_fields
      EXPORTING
        it_f4 = it_f4[].

    SET HANDLER: lcl_event_handler=>on_hotsopt_click FOR g_grid.
    SET HANDLER: lcl_event_handler=>on_f4 FOR g_grid.
    SET HANDLER: lcl_event_handler=>handle_toolbar FOR g_grid.
    SET HANDLER: lcl_event_handler=>handle_user_command FOR  g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

    CREATE OBJECT event.
    SET HANDLER event->handle_data_changed FOR g_grid.
    SET HANDLER event->handle_data_changed_finished FOR g_grid.



  ELSE.
    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = it_fcat.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: v_refresh TYPE c.

  CLEAR: v_refresh.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN '&GERAR'.
      PERFORM gerar_pre_lanc_writer.
      v_refresh = abap_true.

*-CS2021000595 - 22.06.2021 - JT - inicio
    WHEN '&GERAR_AUT'.
      PERFORM gerar_pre_lanc_automatico.
      v_refresh = abap_true.
*-CS2021000595 - 22.06.2021 - JT - fim
    WHEN '&PROC'.
      PERFORM processar_documentos.

      v_refresh = abap_true.
    WHEN '&EST'.
      PERFORM estornar_documentos.

      v_refresh = abap_true.
    WHEN 'ATUALIZAR'.
      v_refresh = abap_true.
    WHEN 'PARCEIROS'.
      CALL TRANSACTION 'ZNFW0011' AND SKIP FIRST SCREEN.
    WHEN 'CAD_OPMIC'.
      CALL TRANSACTION 'ZNFW0012' AND SKIP FIRST SCREEN.
  ENDCASE.

  IF v_refresh EQ abap_true.
    PERFORM: selecao_dados.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_nota_mic.

  DATA: lit_series_mic TYPE TABLE OF setleaf.

  RANGES: lra_series_mic FOR zib_nfe_dist_ter-serie.


  CLEAR: lit_series_mic[], lra_series_mic[].

  SELECT *
    FROM setleaf INTO TABLE lit_series_mic
   WHERE setname = 'ZNFW0009_SERIES_MIC'.

  LOOP AT lit_series_mic INTO DATA(lwa_serie_mic).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_serie_mic-valfrom ) TO lra_series_mic.
  ENDLOOP.

  CHECK lra_series_mic[] IS NOT INITIAL.


  LOOP AT numero INTO DATA(w_numero).
    w_numero-low    =  |{ w_numero-low ALPHA = IN }|.
    w_numero-high   =  |{ w_numero-high ALPHA = IN }|.
    MODIFY numero  FROM w_numero.
  ENDLOOP.


  SELECT   bukrs  branch  numero  serie
           dt_emissao  forne_ie  forne_cnpj  forne_cpf chave_nfe
           destino_cnpj destino_ie  cancel f_transporte
           pc_partiner lr_partiner bukrs_e branch_e
    FROM zib_nfe_dist_ter  INTO TABLE it_zib_nfe_dist_ter
    WHERE ( bukrs_e    IN bukrs OR bukrs IN bukrs )
    AND   dt_emissao IN dt_emis
    AND   serie      IN lra_series_mic
*    AND   cancel     NE 'X'
    AND   numero     IN numero.

  CHECK it_zib_nfe_dist_ter[] IS NOT INITIAL.

  LOOP AT it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter.
    PERFORM f_tratar_ir USING wa_zib_nfe_dist_ter-forne_ie.
    PERFORM f_tratar_ir USING wa_zib_nfe_dist_ter-destino_ie.
    MODIFY it_zib_nfe_dist_ter FROM wa_zib_nfe_dist_ter.
  ENDLOOP.

  SELECT  *
    FROM lfa1 INTO TABLE it_lfa1
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE stcd1 EQ it_zib_nfe_dist_ter-forne_cnpj
     AND ktokk EQ 'ZFIC'.

*-CS2021000595 - 22.06.2021 - JT - inicio
  SELECT  *
    FROM lfa1 APPENDING TABLE it_lfa1
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE stcd2 EQ it_zib_nfe_dist_ter-forne_cpf
     AND ktokk EQ 'ZFIC'.
*-CS2021000595 - 22.06.2021 - JT - fim

  LOOP AT it_lfa1 INTO wa_lfa1.
    PERFORM f_tratar_ir USING wa_lfa1-stcd3.
    MODIFY it_lfa1 FROM wa_lfa1.
  ENDLOOP.

  SELECT  *
    FROM lfa1 INTO TABLE tg_lfa1
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE stcd1 EQ it_zib_nfe_dist_ter-destino_cnpj.

  SELECT  *
    FROM kna1 INTO TABLE tg_kna1
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE stcd1 EQ it_zib_nfe_dist_ter-destino_cnpj.

  LOOP AT tg_lfa1 INTO wg_lfa1.
    PERFORM f_tratar_ir USING wg_lfa1-stcd3.
    MODIFY tg_lfa1 FROM wg_lfa1.
  ENDLOOP.

  LOOP AT tg_kna1 INTO wg_kna1.
    PERFORM f_tratar_ir USING wg_kna1-stcd3.
    MODIFY tg_kna1 FROM wg_kna1.
  ENDLOOP.

  SELECT  *
    FROM zfiwrt0008   INTO TABLE it_zfi08
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE access_key      EQ it_zib_nfe_dist_ter-chave_nfe
     AND tcode_org       EQ 'ZNFW0009'
     AND docs_estornados EQ abap_false
     AND loekz           EQ abap_false.

  PERFORM f_selecao_externa(zwrr0004) TABLES it_zfi08 CHANGING it_saida_znfw0005.

  IF it_zfi08[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt1000 INTO TABLE tg_1000
       FOR ALL ENTRIES IN it_zfi08
     WHERE seq_lcto = it_zfi08-seq_lcto.
  ENDIF.

*---------------------------------------------------------------------
*SET ZNFW0009_DEP_CFOP_OPER
*---------------------------------------------------------------------
*** CS2020001418 - Inicio - CSB
*  SELECT *
*    FROM setleaf INTO TABLE tg_set_dep_cfop_oper
*   WHERE setname EQ 'ZNFW0009_DEP_CFOP_OPER'.
*
*  IF tg_set_dep_cfop_oper[] IS NOT INITIAL.
*    SELECT *
*      FROM setlinet INTO CORRESPONDING FIELDS OF TABLE tg_set_item_cfop_oper
*       FOR ALL ENTRIES IN tg_set_dep_cfop_oper
*     WHERE setname = tg_set_dep_cfop_oper-setname
*       AND lineid  = tg_set_dep_cfop_oper-lineid.
*
*    IF tg_set_item_cfop_oper[] IS NOT INITIAL.
*
*      LOOP AT tg_set_item_cfop_oper.
*        tg_set_item_cfop_oper-operacao = tg_set_item_cfop_oper-descript.
*        MODIFY tg_set_item_cfop_oper.
*      ENDLOOP.
*
*      SELECT * INTO TABLE @DATA(it_zfiwrt0001)
*        FROM zfiwrt0001
*         FOR ALL ENTRIES IN @tg_set_item_cfop_oper
*       WHERE operacao EQ @tg_set_item_cfop_oper-operacao.
*
*      SORT it_zfiwrt0001 BY operacao.
*      DELETE ADJACENT DUPLICATES FROM it_zfiwrt0001 COMPARING operacao.
*    ENDIF.
*  ENDIF.

  SELECT *
    FROM zfiwrt0025 INTO TABLE it_zfiwrt0025
    WHERE bukrs IN bukrs.

  IF it_zfiwrt0025[] IS NOT INITIAL.

    SELECT * INTO TABLE @DATA(it_zfiwrt0001)
      FROM zfiwrt0001
       FOR ALL ENTRIES IN @it_zfiwrt0025
     WHERE operacao EQ @it_zfiwrt0025-operacao.

    SORT it_zfiwrt0001 BY operacao.
    DELETE ADJACENT DUPLICATES FROM it_zfiwrt0001 COMPARING operacao.
  ENDIF.
*** CS2020001418 - Fim - CSB

  IF it_zfiwrt0001[] IS NOT INITIAL.

    SELECT *
      FROM j_1bad INTO TABLE @DATA(lit_j1bad)
      FOR ALL ENTRIES IN @it_zfiwrt0001
     WHERE parvw EQ @it_zfiwrt0001-parvw.
  ENDIF.

  SELECT SINGLE * FROM zfiwrt0024 INTO wa_zfi0024.

  LOOP AT it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter.

    CLEAR:  wa_saida, wa_zib_nfe_dist_itm, wa_zfi08, wl_saida_znfw0005.

    wa_saida-bukrs        = wa_zib_nfe_dist_ter-bukrs.

    IF wa_saida-bukrs IS INITIAL.
      wa_saida-bukrs        = wa_zib_nfe_dist_ter-bukrs_e.
    ENDIF.

    wa_saida-branch       = wa_zib_nfe_dist_ter-branch.

    IF wa_saida-branch IS INITIAL.
      wa_saida-branch       = wa_zib_nfe_dist_ter-branch_e.
    ENDIF.

    wa_saida-numero       = |{ wa_zib_nfe_dist_ter-numero ALPHA = OUT }|.
    wa_saida-serie        = wa_zib_nfe_dist_ter-serie.
    wa_saida-dt_emissao   = wa_zib_nfe_dist_ter-dt_emissao.
    wa_saida-forne_ie     = wa_zib_nfe_dist_ter-forne_ie.
    wa_saida-forne_cnpj   = wa_zib_nfe_dist_ter-forne_cnpj.
    wa_saida-forne_cpf    = wa_zib_nfe_dist_ter-forne_cpf.
    wa_saida-chave_nfe    = wa_zib_nfe_dist_ter-chave_nfe.

    SELECT SINGLE *
     FROM zib_nfe_dist_itm
     INTO wa_zib_nfe_dist_itm
    WHERE chave_nfe EQ wa_zib_nfe_dist_ter-chave_nfe.

    wa_saida-prod_cfop    =  wa_zib_nfe_dist_itm-prod_cfop.

*** CS2020001418 - Inicio -
***   READ TABLE tg_set_dep_cfop_oper WITH KEY valfrom = wa_saida-prod_cfop.
***      READ TABLE tg_set_item_cfop_oper WITH KEY setname = tg_set_dep_cfop_oper-setname
***                                                lineid  = tg_set_dep_cfop_oper-lineid.
***     IF sy-subrc EQ 0.
***        wa_saida-operacao = tg_set_item_cfop_oper-operacao.
***      ENDIF.

*    READ TABLE it_zfiwrt0001 WITH KEY operacao = wa_saida-operacao BINARY SEARCH INTO DATA(wa_zfiwrt0001).
*    IF sy-subrc IS INITIAL AND wa_zfiwrt0001-aviso_rec EQ 'S'.
*      IF wa_zfi0024 IS NOT INITIAL.
*        wa_saida-f_transporte = wa_zfi0024-cod_sb.
*        wa_saida-pc_partiner  = wa_zfi0024-cod_pc.
*        wa_saida-lr_partiner  = wa_zfi0024-cod_lr.
*
**      wa_saida-f_transporte = wa_zib_nfe_dist_ter-f_transporte.
**      wa_saida-pc_partiner  = wa_zib_nfe_dist_ter-pc_partiner.
**      wa_saida-lr_partiner  = wa_zib_nfe_dist_ter-lr_partiner.
*      ENDIF.
*    ENDIF.
*** CS2020001418 - Fim

    DATA(ck_aviso_receb_enabled) = abap_true.
    READ TABLE it_zfi08 INTO wa_zfi08 WITH KEY access_key =  wa_zib_nfe_dist_ter-chave_nfe.
    IF sy-subrc = 0.
      wa_saida-bukrs_lcto   = wa_zfi08-bukrs.
      wa_saida-budat_lcto   = wa_zfi08-budat.
      wa_saida-bldat_lcto   = wa_zfi08-bldat.
      wa_saida-obj_key      = wa_zfi08-obj_key.
      wa_saida-seq_lcto     = wa_zfi08-seq_lcto.
      wa_saida-seq_lcto_out = |{  wa_zfi08-seq_lcto ALPHA = OUT }|.

*** CS2020001418 ZNFW0009 - Inicio
      wa_saida-loc_carrega     = wa_zfi08-loc_carrega.

      READ TABLE it_zfiwrt0025 WITH KEY cfop = wa_saida-prod_cfop
                                  loc_carrega = wa_saida-loc_carrega
                                  bukrs = wa_saida-bukrs .

      IF sy-subrc EQ 0.
        wa_saida-operacao = it_zfiwrt0025-operacao.

        READ TABLE it_zfiwrt0001 WITH KEY operacao = wa_saida-operacao BINARY SEARCH INTO DATA(wa_zfiwrt0001).
        IF sy-subrc IS INITIAL AND wa_zfiwrt0001-aviso_rec EQ 'S'.
          IF wa_zfi0024 IS NOT INITIAL.
            wa_saida-f_transporte = wa_zfi0024-cod_sb.
            wa_saida-pc_partiner  = wa_zfi0024-cod_pc.
            wa_saida-lr_partiner  = wa_zfi0024-cod_lr.
          ENDIF.
        ENDIF.
      ENDIF.

*      IF wa_saida-loc_carrega IS NOT INITIAL.
*        APPEND VALUE #( fieldname = 'LOC_CARREGA'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
*      ELSE.
*        APPEND VALUE #( fieldname = 'LOC_CARREGA'  style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
*      ENDIF.
*** CS2020001418 ZNFW0009 - fim

      APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      ck_aviso_receb_enabled = abap_false.
    ELSEIF wa_zfiwrt0001-aviso_rec NE 'S'.
      APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      ck_aviso_receb_enabled = abap_false.
    ENDIF.

    IF wa_saida-seq_lcto NOT IN seq_lcto.
      CONTINUE.
    ENDIF.

    " Novas colunas CS2020*218
    SELECT  *
      FROM zib_nfe_dist_itm
      INTO TABLE @DATA(it_zib_nfe_dist_itm)
     WHERE chave_nfe EQ @wa_zib_nfe_dist_ter-chave_nfe.

    DESCRIBE TABLE it_zib_nfe_dist_itm LINES DATA(l_lines).
    CLEAR: wa_saida-prod_qtd_comerci,wa_saida-prod_vlr_und_com, wa_saida-prod_vlr_total_b, wa_saida-prod_pedido_comp.
    DATA(_fator2) = 1000.
    LOOP AT  it_zib_nfe_dist_itm INTO wa_zib_nfe_dist_itm.
      _fator2 = 1.
      IF wa_zib_nfe_dist_itm-prod_und_comerci = 'TO'.
        _fator2 = 1000.
      ENDIF.
      IF l_lines = 1.
        wa_saida-prod_qtd_comerci = wa_zib_nfe_dist_itm-prod_qtd_comerci * _fator2.
        wa_saida-prod_vlr_und_com = wa_zib_nfe_dist_itm-prod_vlr_und_com / _fator2.
      ENDIF.
      ADD wa_zib_nfe_dist_itm-prod_vlr_total_b TO wa_saida-prod_vlr_total_b.
      IF wa_zib_nfe_dist_itm-prod_pedido_comp IS NOT INITIAL.
        wa_saida-prod_pedido_comp = wa_zib_nfe_dist_itm-prod_pedido_comp.
      ENDIF.
    ENDLOOP.

    READ TABLE it_zfiwrt0001 INTO DATA(lwa_zfit0001) WITH KEY operacao = wa_saida-operacao.
    IF sy-subrc EQ 0.
      READ TABLE lit_j1bad INTO DATA(lwa_j_1bad) WITH KEY parvw = lwa_zfit0001-parvw.
      IF sy-subrc EQ 0.
        CASE lwa_j_1bad-partyp.
          WHEN 'C'.
            READ TABLE tg_kna1 INTO wg_kna1 WITH KEY stcd1 = wa_zib_nfe_dist_ter-destino_cnpj
                                                     stcd3 = wa_zib_nfe_dist_ter-destino_ie.
            IF sy-subrc = 0.
              wa_saida-destino_cnpj  = wg_kna1-stcd1.
              wa_saida-destino_ie    = wg_kna1-stcd3.
              wa_saida-destino_lifnr = wg_kna1-kunnr.
              wa_saida-destino_name1 = wg_kna1-name1.
            ENDIF.
          WHEN 'V'.
            READ TABLE tg_lfa1 INTO wg_lfa1 WITH KEY stcd1 = wa_zib_nfe_dist_ter-destino_cnpj
                                                     stcd3 = wa_zib_nfe_dist_ter-destino_ie.
            IF sy-subrc = 0.
              wa_saida-destino_cnpj  = wg_lfa1-stcd1.
              wa_saida-destino_ie    = wg_lfa1-stcd3.
              wa_saida-destino_lifnr = wg_lfa1-lifnr.
              wa_saida-destino_name1 = wg_lfa1-name1.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

    READ TABLE it_saida_znfw0005 INTO wl_saida_znfw0005 WITH KEY seq_lcto = wa_zfi08-seq_lcto.
    IF sy-subrc EQ 0.
      wa_saida-status           = wl_saida_znfw0005-status.
      wa_saida-status_proc      = wl_saida_znfw0005-status_proc.

      "Documentos
      wa_saida-docnum           = wl_saida_znfw0005-docnum.
      wa_saida-docnum_flag      = wl_saida_znfw0005-docnum_flag.
      wa_saida-doc_contab       = wl_saida_znfw0005-doc_contab.
      wa_saida-doc_contab_flag  = wl_saida_znfw0005-doc_contab_flag.
      wa_saida-belnr            = wl_saida_znfw0005-belnr.
      wa_saida-belnr_flag       = wl_saida_znfw0005-belnr_flag.
      wa_saida-vbeln_r          = wl_saida_znfw0005-vbeln_r.
      wa_saida-vbeln_flag       = wl_saida_znfw0005-vbeln_flag.
      wa_saida-doc_mat          = wl_saida_znfw0005-doc_mat.
      wa_saida-doc_mat_flag     = wl_saida_znfw0005-doc_mat_flag.
      wa_saida-doc_transf       = wl_saida_znfw0005-doc_transf.
      wa_saida-doc_transf_flag  = wl_saida_znfw0005-doc_transf_flag.
      wa_saida-nivel_aprov      = wl_saida_znfw0005-nivel_aprov .
      wa_saida-aprov            = wl_saida_znfw0005-aprov.
      wa_saida-bloqueado = ' '.
    ELSE.
      wa_saida-bloqueado        = 'X'.     "BLOQUEAR PROCESSAMOS DO REGISTRO
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd1 = wa_zib_nfe_dist_ter-forne_cnpj
                                             stcd3 = wa_zib_nfe_dist_ter-forne_ie.
    IF sy-subrc = 0.
      wa_saida-lifnr    = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      wa_saida-name1    = wa_lfa1-name1.

      SELECT SINGLE *
        FROM j_1bbranch INTO @DATA(wa_j_1bbranch)
       WHERE branch EQ @wa_lfa1-lifnr+6(4).

      IF sy-subrc EQ 0.
        wa_saida-pbukrs  = wa_j_1bbranch-bukrs.
        wa_saida-pbranch = wa_j_1bbranch-branch.
      ENDIF.
    ENDIF.

*-CS2021000595 - 22.06.2021 - JT - inicio
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd2 = wa_zib_nfe_dist_ter-forne_cpf.
    IF sy-subrc = 0.
      wa_saida-lifnr    = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      wa_saida-name1    = wa_lfa1-name1.

      SELECT SINGLE *
        FROM j_1bbranch INTO wa_j_1bbranch
       WHERE branch EQ wa_lfa1-lifnr+6(4).

      IF sy-subrc EQ 0.
        wa_saida-pbukrs  = wa_j_1bbranch-bukrs.
        wa_saida-pbranch = wa_j_1bbranch-branch.
      ENDIF.
    ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

    IF ck_aviso_receb_enabled EQ abap_true.
      READ TABLE it_zfiwrt0001 WITH KEY operacao = wa_saida-operacao BINARY SEARCH INTO DATA(wa_zfiwrt0001_).
      IF sy-subrc IS INITIAL AND wa_zfiwrt0001_-aviso_rec EQ 'S'.
        APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
        APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
        APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
      ENDIF.
    ENDIF.

    IF wa_saida-docnum IS NOT INITIAL.

      "Itens da nota fiscal
      SELECT SINGLE docnum, itmnum, reftyp, refkey, refitm FROM j_1bnflin
        INTO @DATA(wa_lin)
        WHERE docnum = @wa_saida-docnum
          AND reftyp = 'ZW'.

      IF wa_lin-refkey IS NOT INITIAL.

        "Tabela de Lançamento  Fiscais NF.WRITER
        SELECT SINGLE seq_lcto, ebeln, ebelp FROM zfiwrt0008
          INTO @DATA(wa_008)
          WHERE bukrs     IN @bukrs
            AND seq_lcto = @wa_lin-refkey.

        " Buscar número do pedido
        wa_saida-ebeln = wa_008-ebeln.
        wa_saida-ebelp = wa_008-ebelp.

        CLEAR: wa_lin, wa_008.

      ENDIF.

    ENDIF.

    wa_saida-cancel = wa_zib_nfe_dist_ter-cancel.

    IF wa_saida-cancel IS NOT INITIAL.
      wa_saida-color_line = 'C610'.
    ENDIF.

    IF pen IS NOT INITIAL.

      IF wa_saida-seq_lcto IS INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.

    ELSEIF  ger IS NOT INITIAL.

      IF wa_saida-seq_lcto IS NOT INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.

    ELSEIF am IS NOT INITIAL.
      APPEND wa_saida TO it_saida.
    ENDIF.

  ENDLOOP.

  SORT it_saida BY dt_emissao.

  IF it_saida_mic[] IS NOT INITIAL.
    LOOP AT it_saida INTO wa_saida.
      v_index = sy-tabix.
      READ TABLE it_saida_mic INTO wa_saida_mic WITH KEY chave_nfe = wa_saida-chave_nfe.

      IF wa_saida_mic-loc_carrega IS NOT INITIAL.
        wa_saida-loc_carrega    = wa_saida_mic-loc_carrega.
        wa_saida-operacao       = wa_saida_mic-operacao.
        wa_saida-destino_cnpj   = wa_saida_mic-destino_cnpj.
        wa_saida-destino_ie     = wa_saida_mic-destino_ie.
        wa_saida-destino_lifnr  = wa_saida_mic-destino_lifnr.
        wa_saida-destino_name1  = wa_saida_mic-destino_name1.
        wa_saida-f_transporte   = wa_saida_mic-f_transporte.
        wa_saida-lr_partiner    = wa_saida_mic-lr_partiner.
        wa_saida-pc_partiner    = wa_saida_mic-pc_partiner.

        MODIFY it_saida FROM wa_saida INDEX v_index.
        CLEAR: wa_saida.
      ENDIF.
    ENDLOOP.
    CLEAR: it_saida_mic[].
  ENDIF.

ENDFORM.


FORM f_tratar_ir USING p_ie.

  DATA: vl_ie_num TYPE i.

  CHECK p_ie IS NOT INITIAL.

  CLEAR vl_ie_num.

  REPLACE ALL OCCURRENCES OF '.' IN p_ie WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF '/' IN p_ie WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF '\' IN p_ie WITH '' IGNORING CASE.
  REPLACE ALL OCCURRENCES OF '-' IN p_ie WITH '' IGNORING CASE.

  CONDENSE p_ie NO-GAPS.

  TRY.
      vl_ie_num = p_ie.
      p_ie      = vl_ie_num.
      CONDENSE p_ie NO-GAPS.
    CATCH cx_sy_conversion_no_number.
    CATCH cx_sy_conversion_overflow.
  ENDTRY.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM alv.

  CLEAR: it_fcat[].

  PERFORM preenche_cat USING:
          'STATUS'                  'Status'               '10'      ''     ''    ''     'C' '' ''.

  IF p_onf IS NOT INITIAL.  "Outras NF.

    PERFORM preenche_cat USING:
          'BUKRS'                   'Empresa'              '07'      ''     ''    ''     ''  '' '',
          'BRANCH'                  'Filial'               '06'      ''     ''    ''     ''  '' ''.

  ENDIF.

  PERFORM preenche_cat USING:

          'NUMERO'                  'NF-e'                 '09'      ''     ''    ''     '' '' '',
          'SERIE'                   'Serie'                '04'      ''     ''    ''     '' '' '',

          'PROD_QTD_COMERCI'        'Qtde'                 '12'      ''     ''    ''     '' '' '',
          'PROD_VLR_UND_COM'        'Vlr.Unit'             '12'      ''     ''    ''     '' '' '',
          'PROD_VLR_TOTAL_B'        'Vlr.Total'            '15'      ''     ''    ''     '' '' '',
          'PROD_PEDIDO_COMP'        'Pedido'               '10'      ''     ''    ''     '' '' '',

          'DT_EMISSAO'              'Data Emissão'         '12'      ''     ''    ''     '' '' '',
          'PROD_CFOP'               'CFOP'                 '05'      ''     ''    ''     '' '' '',
          'FORNE_CPF'               'CPF Emissor'          '14'      ''     ''    ''     '' '' '',
          'FORNE_CNPJ'              'CNPJ Emissor'         '14'      ''     ''    ''     '' '' '',
          'FORNE_IE'                'IE Emissor'           '15'      ''     ''    ''     '' '' '',
          'LIFNR'                   'Cód. Emissor'         '12'      ''     ''    ''     '' '' 'ALPHA',
          'NAME1'                   'Nome Emissor'         '35'      ''     ''    ''     '' '' ''.

  IF p_nm IS NOT INITIAL.  "Notas Mic.

    PERFORM preenche_cat USING:
            'DESTINO_CNPJ'          'CNPJ Dest.'           '14'      ''     ''    ''     '' ''  '',
            'DESTINO_IE'            'IE Dest.'             '15'      ''     ''    ''     '' ''  '',
            'DESTINO_LIFNR'         'Cód. Dest.'           '12'      ''     ''    ''     '' ''  '',
            'DESTINO_NAME1'         'Nome Dest.'           '35'      ''     ''    ''     '' ''  '',
            'LOC_CARREGA'           'Local Carregamento'   '18'      ''     ''    ''     '' 'X'  '',
            'OPERACAO'              'Código Operação'      '15'      ''     ''    ''     '' ''  ''.
  ENDIF.


  PERFORM preenche_cat USING:
          'CHAVE_NFE'               'Chave NF-e'           '44'      ''     ''    ''     ''  '' '',
          'F_TRANSPORTE'            'Agente de Frete'      '10'      ''     ''    ''     ''  '' 'ALPHA',
          'PC_PARTINER'             'Ponto Coleta'         '10'      ''     ''    ''     ''  '' 'ALPHA',
          'LR_PARTINER'             'Local Entrega'        '10'      ''     ''    ''     ''  '' 'ALPHA',
*          'F_TRANSPORTE'            'Agente de Frete'      '10'      ''     ''    ''     ''  'X' 'ALPHA',
*          'PC_PARTINER'             'Ponto Coleta'         '10'      ''     ''    ''     ''  'X' 'ALPHA',
*          'LR_PARTINER'             'Local Entrega'        '10'      ''     ''    ''     ''  'X' 'ALPHA',
          'SEQ_LCTO_OUT'            'Seq.Lcto'             '08'      ''     'X'   ''     ''  '' ''.

  PERFORM preenche_cat USING:
        'EBELN'            'Nro. Pedido'         '15'      ''     'X'   ''     ''  '' '',
        'EBELP'            'Nro. Pedido'         '08'      ''     ' '   ''     ''  '' ''.

  PERFORM preenche_cat USING:
        'NIVEL_APROV'             'Nivel Aprovador'      '15'      ''     ''    ''     ''  '' '',
        'APROV'                   'Aprovador'            '10'      ''     ''    ''     ''  '' '',
        'DOC_MAT_FLAG'            'Doc.Material'         '15'      ''     'X'    ''    'C' '' '',
        'DOCNUM_FLAG'             'Doc.Num'              '15'      ''     'X'    ''    'C' '' '',
        'BELNR_FLAG'              'Doc Vd. Imob.'        '15'      ''     'X'    ''    'C' '' '',
        'VBELN_FLAG'              'Remessa'              '15'      ''     'X'    ''    'C' '' '',
        'DOC_TRANSF_FLAG'         'Doc.Transf.Imob'      '15'      ''     'X'    ''    'C' '' '',
        'DOC_CONTAB_FLAG'         'Doc.Contabil'         '15'      ''     'X'    ''    'C' '' ''.

  READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>) WITH KEY fieldname = 'EBELP'.
  IF <fs_fcat> IS ASSIGNED.
    <fs_fcat>-tech   = abap_true.
    <fs_fcat>-no_out = abap_true.
  ENDIF.

  READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat_aux>) WITH KEY fieldname = 'LOC_CARREGA'.
  IF <fs_fcat_aux> IS ASSIGNED AND ger = 'X'.
    <fs_fcat_aux>-edit  = ''.
  ENDIF.

  CALL SCREEN 0100.

ENDFORM.


FORM preenche_cat USING VALUE(p_campo)
                        VALUE(p_desc)
                        VALUE(p_tam)
                        VALUE(p_zero)
                        VALUE(p_hot)
                        VALUE(p_sum)
                        VALUE(p_just)
                        VALUE(p_edit)
                        VALUE(p_alpha).

  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-outputlen = p_tam.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-do_sum    = p_sum.
  wl_fcat-just      = p_just.
  wl_fcat-edit      = p_edit. "CS2020001418 ZNFW0009
  wl_fcat-convexit  = p_alpha.

*** CS2020001418 ZNFW0009 - Inicio - CSB
  IF p_campo EQ 'LOC_CARREGA' .
    wl_fcat-f4availabl = 'X'.
  ENDIF.
*** CS2020001418 ZNFW0009 - Fim - CSB

  APPEND wl_fcat TO  it_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_OUTRAS_NF
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados_outras_nf.

  DATA: lit_series_mic TYPE TABLE OF setleaf.

  RANGES: lra_series_mic FOR zib_nfe_dist_ter-serie.

  CLEAR: lit_series_mic[], lra_series_mic[].

  SELECT *
    FROM setleaf INTO TABLE lit_series_mic
   WHERE setname = 'ZNFW0009_SERIES_MIC'.

  LOOP AT lit_series_mic INTO DATA(lwa_serie_mic).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = lwa_serie_mic-valfrom ) TO lra_series_mic.
  ENDLOOP.

  CHECK lra_series_mic[] IS NOT INITIAL.

  SELECT  bukrs branch  numero  serie
          dt_emissao  forne_ie forne_cnpj forne_cpf chave_nfe
          destino_cnpj  destino_ie   cancel f_transporte
          pc_partiner lr_partiner
    FROM zib_nfe_dist_ter  INTO TABLE it_zib_nfe_dist_ter
      WHERE dt_emissao IN dt_emis
       AND  serie      NOT IN lra_series_mic
       AND  forne_cnpj IN p_cnpj
*-CS2021000595 - 22.06.2021 - JT - inicio
       AND  forne_cpf  IN p_cpf
*-CS2021000595 - 22.06.2021 - JT - fim

       AND  chave_nfe  IN p_chave
       AND  cancel     NE 'X'
       AND  numero     IN numero.

  CHECK it_zib_nfe_dist_ter[] IS NOT INITIAL.

  LOOP AT it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter.

    PERFORM f_tratar_ir USING wa_zib_nfe_dist_ter-forne_ie.
    PERFORM f_tratar_ir USING wa_zib_nfe_dist_ter-destino_ie.

    MODIFY it_zib_nfe_dist_ter FROM wa_zib_nfe_dist_ter.
  ENDLOOP.

  DATA(it_zib_nfe_dist_ter_aux) = it_zib_nfe_dist_ter[].
  DELETE it_zib_nfe_dist_ter_aux WHERE forne_cnpj IS INITIAL.

  IF it_zib_nfe_dist_ter_aux[] IS NOT INITIAL.
    SELECT  *
      FROM lfa1 INTO TABLE it_lfa1
       FOR ALL ENTRIES IN it_zib_nfe_dist_ter_aux
     WHERE stcd1 EQ it_zib_nfe_dist_ter_aux-forne_cnpj.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - inicio

  it_zib_nfe_dist_ter_aux = it_zib_nfe_dist_ter[].
  DELETE it_zib_nfe_dist_ter_aux WHERE forne_cpf IS INITIAL.

  IF it_zib_nfe_dist_ter_aux[] IS NOT INITIAL.
    SELECT  *
      FROM lfa1 APPENDING TABLE it_lfa1
       FOR ALL ENTRIES IN it_zib_nfe_dist_ter_aux
     WHERE stcd2 EQ it_zib_nfe_dist_ter_aux-forne_cpf.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

  LOOP AT it_lfa1 INTO wa_lfa1.
    PERFORM f_tratar_ir USING wa_lfa1-stcd3.
    MODIFY it_lfa1 FROM wa_lfa1.
  ENDLOOP.

  SELECT  *
    FROM zfiwrt0008 INTO TABLE it_zfi08
     FOR ALL ENTRIES IN it_zib_nfe_dist_ter
   WHERE access_key      EQ it_zib_nfe_dist_ter-chave_nfe
     AND tcode_org       EQ 'ZNFW0009'
     AND docs_estornados EQ abap_false
     AND loekz           EQ abap_false.

  PERFORM f_selecao_externa(zwrr0004) TABLES it_zfi08
                                    CHANGING it_saida_znfw0005.

  IF it_zfi08[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt1000 INTO TABLE tg_1000
       FOR ALL ENTRIES IN it_zfi08
     WHERE seq_lcto = it_zfi08-seq_lcto.

    SELECT * INTO TABLE @DATA(it_zfiwrt0001)
      FROM zfiwrt0001
       FOR ALL ENTRIES IN @it_zfi08
     WHERE operacao EQ @it_zfi08-operacao.
  ENDIF.



  SELECT * APPENDING TABLE @it_zfiwrt0001
    FROM zfiwrt0001
   WHERE operacao IN @p_ope.

  SORT it_zfiwrt0001 BY operacao.
  DELETE ADJACENT DUPLICATES FROM it_zfiwrt0001 COMPARING operacao.

  LOOP AT it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter.

    CLEAR: wa_saida, wa_zib_nfe_dist_itm, wa_zfi08, wl_saida_znfw0005.

    wa_saida-bukrs        = wa_zib_nfe_dist_ter-bukrs.
    wa_saida-branch       = wa_zib_nfe_dist_ter-branch.
    wa_saida-numero       = |{ wa_zib_nfe_dist_ter-numero ALPHA = OUT }|.
    wa_saida-serie        = wa_zib_nfe_dist_ter-serie.
    wa_saida-dt_emissao   = wa_zib_nfe_dist_ter-dt_emissao.
    wa_saida-f_transporte = wa_zib_nfe_dist_ter-f_transporte.
    wa_saida-pc_partiner  = wa_zib_nfe_dist_ter-pc_partiner.
    wa_saida-lr_partiner  = wa_zib_nfe_dist_ter-lr_partiner.

    PERFORM f_tratar_ir USING wa_zib_nfe_dist_ter-forne_ie.

    wa_saida-forne_ie    =  wa_zib_nfe_dist_ter-forne_ie.
    wa_saida-forne_cnpj  =  wa_zib_nfe_dist_ter-forne_cnpj.
    wa_saida-forne_cpf   =  wa_zib_nfe_dist_ter-forne_cpf.
    wa_saida-chave_nfe   =  wa_zib_nfe_dist_ter-chave_nfe.

    DATA(ck_aviso_receb_enabled) = abap_true.
    READ TABLE it_zfi08 INTO wa_zfi08 WITH KEY access_key =  wa_zib_nfe_dist_ter-chave_nfe.
    IF sy-subrc = 0.
      wa_saida-bukrs_lcto   = wa_zfi08-bukrs.
      wa_saida-budat_lcto   = wa_zfi08-budat.
      wa_saida-bldat_lcto   = wa_zfi08-bldat.
      wa_saida-obj_key      = wa_zfi08-obj_key.
      wa_saida-seq_lcto     = wa_zfi08-seq_lcto.
      wa_saida-operacao     = wa_zfi08-operacao.
      wa_saida-loc_carrega  = wa_zfi08-loc_carrega.
      wa_saida-seq_lcto_out = |{  wa_zfi08-seq_lcto ALPHA = OUT }|.
      APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO wa_saida-cellstyles.
      ck_aviso_receb_enabled = abap_false.
    ELSE.
      wa_saida-operacao = p_ope-low.
    ENDIF.

    IF wa_saida-seq_lcto NOT IN seq_lcto.
      CONTINUE.
    ENDIF.

    SELECT SINGLE *
      FROM zib_nfe_dist_itm
      INTO wa_zib_nfe_dist_itm
     WHERE chave_nfe EQ wa_zib_nfe_dist_ter-chave_nfe.

    wa_saida-prod_cfop        =  wa_zib_nfe_dist_itm-prod_cfop.

    READ TABLE it_saida_znfw0005 INTO wl_saida_znfw0005 WITH KEY seq_lcto = wa_zfi08-seq_lcto.
    IF sy-subrc EQ 0.
      wa_saida-status           = wl_saida_znfw0005-status.
      wa_saida-status_proc      = wl_saida_znfw0005-status_proc.
      wa_saida-docnum           = wl_saida_znfw0005-docnum.
      wa_saida-docnum_flag      = wl_saida_znfw0005-docnum_flag.
      wa_saida-doc_contab       = wl_saida_znfw0005-doc_contab.
      wa_saida-doc_contab_flag  = wl_saida_znfw0005-doc_contab_flag.
      wa_saida-belnr            = wl_saida_znfw0005-belnr.
      wa_saida-belnr_flag       = wl_saida_znfw0005-belnr_flag.
      wa_saida-vbeln_r          = wl_saida_znfw0005-vbeln_r.
      wa_saida-vbeln_flag       = wl_saida_znfw0005-vbeln_flag.
      wa_saida-doc_mat          = wl_saida_znfw0005-doc_mat.
      wa_saida-doc_mat_flag     = wl_saida_znfw0005-doc_mat_flag.
      wa_saida-doc_transf       = wl_saida_znfw0005-doc_transf.
      wa_saida-doc_transf_flag  = wl_saida_znfw0005-doc_transf_flag.
      wa_saida-nivel_aprov      = wl_saida_znfw0005-nivel_aprov .
      wa_saida-aprov            = wl_saida_znfw0005-aprov.
      wa_saida-bloqueado        = abap_false.
      ck_aviso_receb_enabled    = abap_false.
    ELSE.
      wa_saida-bloqueado        = abap_true.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd1 = wa_zib_nfe_dist_ter-forne_cnpj
                                             stcd3 = wa_zib_nfe_dist_ter-forne_ie.
    IF sy-subrc = 0.
      wa_saida-lifnr = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      wa_saida-name1 = wa_lfa1-name1.
    ENDIF.

*-CS2021000595 - 22.06.2021 - JT - inicio
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd2 = wa_zib_nfe_dist_ter-forne_cpf
                                             stcd3 = wa_zib_nfe_dist_ter-forne_ie.
    IF sy-subrc = 0.
      wa_saida-lifnr = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      wa_saida-name1 = wa_lfa1-name1.
    ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

    IF ck_aviso_receb_enabled EQ abap_true.
      READ TABLE it_zfiwrt0001 WITH KEY operacao = wa_saida-operacao BINARY SEARCH INTO DATA(wa_zfiwrt0001).
      IF sy-subrc IS INITIAL AND wa_zfiwrt0001-aviso_rec EQ 'S'.
        APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
        APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
        APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO wa_saida-cellstyles.
      ENDIF.
    ENDIF.

    "Buscar pedido
    IF wa_saida-docnum IS NOT INITIAL.

      "Itens da nota fiscal
      SELECT SINGLE docnum, itmnum, reftyp, refkey, refitm FROM j_1bnflin
        INTO @DATA(wa_lin)
        WHERE docnum = @wa_saida-docnum
          AND reftyp = 'ZW'.

      IF wa_lin-refkey IS NOT INITIAL.

        "Tabela de Lançamento  Fiscais NF.WRITER
        SELECT SINGLE seq_lcto, ebeln, ebelp FROM zfiwrt0008
          INTO @DATA(wa_008)
          WHERE bukrs     IN @bukrs
            AND seq_lcto = @wa_lin-refkey.

        " Buscar número do pedido
        wa_saida-ebeln = wa_008-ebeln.
        wa_saida-ebelp = wa_008-ebelp.

        CLEAR: wa_lin, wa_008.

      ENDIF.

    ENDIF.

    wa_saida-cancel = wa_zib_nfe_dist_ter-cancel.

    IF wa_saida-cancel IS NOT INITIAL.
      wa_saida-color_line = 'C610'.
    ENDIF.

    IF pen IS NOT INITIAL.
      IF wa_saida-seq_lcto IS INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ELSEIF  ger IS NOT INITIAL.
      IF wa_saida-seq_lcto IS NOT INITIAL.
        APPEND wa_saida TO it_saida.
      ENDIF.
    ELSEIF am IS NOT INITIAL.
      APPEND wa_saida TO it_saida.
    ENDIF.

  ENDLOOP.

  SORT it_saida BY dt_emissao.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_PRE_LANC_WRITER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gera_pre_lancamento USING wa_saida TYPE ty_saida
                               p_modo.
*-----------------------------------------------------
* Modo -> 1 = Pre lancto manual
*         2 = Pre lancto automatico
*-----------------------------------------------------


  DATA: v_bukrs_memo      TYPE zib_nfe_dist_ter-bukrs,
        v_branch_memo     TYPE zib_nfe_dist_ter-branch,
        v_numero_memo     TYPE zib_nfe_dist_ter-numero,
        v_serie_memo      TYPE zib_nfe_dist_ter-serie,
        v_dt_emissao_memo TYPE zib_nfe_dist_ter-dt_emissao,
        v_lifnr_memo      TYPE lfa1-lifnr,
        v_operacao_memo   TYPE zfiwrt0001-operacao,
        v_chave_nfe_memo  TYPE zib_nfe_dist_ter-chave_nfe,
        v_lcto_memo       TYPE c LENGTH 10,
        v_automatico_memo TYPE char1,
        v_agente          TYPE zib_nfe_dist_ter-f_transporte,
        v_coleta          TYPE zib_nfe_dist_ter-pc_partiner,
        v_entrega         TYPE zib_nfe_dist_ter-lr_partiner,
        v_mensagem        TYPE string,
        v_cfop            TYPE c,
        v_loc_carrega     TYPE zfiwrt0008-loc_carrega. "CS2020001418

  FREE:  v_bukrs_memo,      v_branch_memo, v_numero_memo, v_serie_memo,
         v_dt_emissao_memo, v_lifnr_memo,  v_operacao_memo,
         v_chave_nfe_memo,  v_lcto_memo,   v_agente,
         v_coleta,          v_entrega,     v_loc_carrega, v_automatico_memo.

  FREE: MEMORY ID 'EMPRESA',
        MEMORY ID 'FILIAL',
        MEMORY ID 'NFE',
        MEMORY ID 'SERIE',
        MEMORY ID 'DT_EMISSAO',
        MEMORY ID 'IDPARCEIRO',
        MEMORY ID 'OPERACAO',
        MEMORY ID 'CHAVE',
        MEMORY ID 'LANCAMENTO',
        MEMORY ID 'AGENTE',
        MEMORY ID 'COLETA',
        MEMORY ID 'ENTREGA',
        MEMORY ID 'AUTOMATICO',
        MEMORY ID 'LOC_CARREGA'.

  IF p_modo = '2'.
    v_automatico_memo = abap_true.
  ENDIF.

  "Não permite execução de itens cancelados
  IF wa_saida-cancel IS NOT INITIAL.
    IF p_modo = '1'.
      MESSAGE s000(z_les) WITH 'Nota Fiscal cancelada! Operação não permitida!' DISPLAY LIKE 'E'.
    ENDIF.
    EXIT.
  ELSE.

    SELECT SINGLE * FROM zfiwrt0008
      INTO @DATA(wa_zfi08)
     WHERE access_key      EQ @wa_saida-chave_nfe
       AND tcode_org       EQ 'ZNFW0009'
       AND docs_estornados EQ @abap_false
       AND loekz           EQ @abap_false.

    IF sy-subrc = 0.
      IF p_modo = '1'.
        MESSAGE 'Ação não permitida! Já existe um lançamento para a chave informada.' TYPE 'E'.
      ENDIF.
      EXIT.
    ELSE.

      v_agente    = |{ wa_saida-f_transporte ALPHA = IN }|.
      v_coleta    = |{ wa_saida-pc_partiner ALPHA = IN }|.
      v_entrega   = |{ wa_saida-lr_partiner ALPHA = IN }|.

      "Local de entrega
      IF v_entrega IS NOT INITIAL.
        SELECT SINGLE * FROM knb1  INTO @DATA(wl_knb1)
          WHERE kunnr EQ @v_entrega
          AND   bukrs EQ @wa_saida-pbukrs.

        IF sy-subrc NE 0.
          CLEAR v_mensagem.
          IF p_modo = '1'.
            CONCATENATE 'Ação não permitida! Local de Entrega não expandido para empresa' wa_saida-pbukrs INTO v_mensagem SEPARATED BY space.
            MESSAGE v_mensagem TYPE 'E'.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.

      "Ponto de Coleta
      IF v_coleta  IS NOT INITIAL.
        SELECT SINGLE * FROM lfb1 INTO @DATA(wl_lfb1)
          WHERE lifnr EQ @v_coleta
          AND   bukrs EQ @wa_saida-pbukrs.

        IF sy-subrc NE 0.
          CLEAR v_mensagem.
          IF p_modo = '1'.
            CONCATENATE 'Ação não permitida! Ponto de Coleta não expandido para empresa' wa_saida-pbukrs INTO v_mensagem SEPARATED BY space.
            MESSAGE v_mensagem TYPE 'E'.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.

      "Agente de Frete
      CLEAR  wl_lfb1.
      IF v_agente  IS NOT INITIAL.
        SELECT SINGLE * FROM lfb1 INTO wl_lfb1
          WHERE lifnr EQ v_agente
          AND   bukrs EQ wa_saida-pbukrs.

        IF sy-subrc NE 0.
          CLEAR v_mensagem.
          IF p_modo = '1'.
            CONCATENATE 'Ação não permitida! Agente de Frete não expandido para empresa' wa_saida-pbukrs INTO v_mensagem SEPARATED BY space.
            MESSAGE v_mensagem TYPE 'E'.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.

      IF p_ope-low IS NOT INITIAL AND  p_onf IS NOT INITIAL.
        SELECT *
          FROM zfiwrt0006 INTO TABLE @DATA(it_cfop)
         WHERE operacao EQ @p_ope-low.

        LOOP AT it_cfop INTO DATA(wa_cfop).
          IF wa_cfop-cfop+0(4) EQ wa_saida-prod_cfop.
            v_cfop = abap_true.
            EXIT.
          ENDIF.
        ENDLOOP.

*          IF v_cfop NE abap_true.
*            CLEAR v_mensagem.
*            CONCATENATE 'Ação não permitida! CFOP incompatível com a operação selecionada, para nota' wa_saida-numero INTO v_mensagem SEPARATED BY space.
*            MESSAGE v_mensagem TYPE 'E'.
*            EXIT.
*          ENDIF.
      ENDIF.

      IF p_nm IS NOT INITIAL.
        v_bukrs_memo       = wa_saida-pbukrs.
        v_branch_memo      = wa_saida-pbranch.
        v_numero_memo      = wa_saida-numero.
        v_serie_memo       = wa_saida-serie.
        v_dt_emissao_memo  = wa_saida-dt_emissao.
        v_lifnr_memo       = wa_saida-destino_lifnr.
        v_operacao_memo    = wa_saida-operacao.
        v_loc_carrega      = wa_saida-loc_carrega.
        v_chave_nfe_memo   = wa_saida-chave_nfe.
        v_lcto_memo        = 'MIC'.

        v_agente    = |{ wa_saida-f_transporte ALPHA = IN }|.
        v_coleta    = |{ wa_saida-pc_partiner ALPHA = IN }|.
        v_entrega   = |{ wa_saida-lr_partiner ALPHA = IN }|.

        IF v_operacao_memo IS INITIAL.
          IF p_modo = '1'.
            MESSAGE 'Ação não permitida! Operação não parametrizada para o lançamento!' TYPE 'E'.
          ENDIF.
          EXIT.
        ENDIF.

        SELECT SINGLE * INTO @DATA(wa_zfiwrt0001)
          FROM zfiwrt0001
         WHERE operacao EQ @wa_saida-operacao.

        IF sy-subrc NE 0.
          IF p_modo = '1'.
            MESSAGE |Operação { wa_saida-operacao } não encontrada! | TYPE 'I'.
          ENDIF.
          EXIT.
        ENDIF.

        IF wa_zfiwrt0001-aviso_rec EQ 'S' AND wa_saida-chave_nfe IS NOT INITIAL.
          IF wa_saida-f_transporte IS INITIAL OR wa_saida-pc_partiner IS INITIAL OR wa_saida-lr_partiner IS INITIAL.
            IF p_modo = '1'.
              MESSAGE 'Para Operações com Aviso de Recebimento deve ser Preenchido os Parceiros Agente/Coleta/Entrega!' TYPE 'E'.
            ENDIF.
            EXIT.
          ENDIF.
          UPDATE zib_nfe_dist_ter
             SET f_transporte = wa_saida-f_transporte
                 pc_partiner  = wa_saida-pc_partiner
                 lr_partiner  = wa_saida-lr_partiner
           WHERE chave_nfe EQ wa_saida-chave_nfe.
          COMMIT WORK.
        ENDIF.

      ELSEIF p_onf IS NOT INITIAL.

        SELECT SINGLE * INTO wa_zfiwrt0001
          FROM zfiwrt0001
         WHERE operacao EQ wa_saida-operacao.

        IF sy-subrc NE 0.
          IF p_modo = '1'.
            MESSAGE |Operação { wa_saida-operacao } não encontrada! | TYPE 'I'.
          ENDIF.
          EXIT.
        ENDIF.

        v_bukrs_memo       = wa_saida-bukrs.
        v_branch_memo      = wa_saida-branch.
        v_numero_memo      = wa_saida-numero.
        v_serie_memo       = wa_saida-serie.
        v_dt_emissao_memo  = wa_saida-dt_emissao.
        v_lifnr_memo       = wa_saida-lifnr.
        v_operacao_memo    = wa_saida-operacao.
        v_loc_carrega      = wa_saida-loc_carrega.
        v_chave_nfe_memo   = wa_saida-chave_nfe.
        v_lcto_memo        = 'ONF'.
        v_agente    = |{ wa_saida-f_transporte ALPHA = IN }|.
        v_coleta    = |{ wa_saida-pc_partiner ALPHA = IN }|.
        v_entrega   = |{ wa_saida-lr_partiner ALPHA = IN }|.
        IF wa_zfiwrt0001-aviso_rec EQ 'S' AND wa_saida-chave_nfe IS NOT INITIAL.
          IF wa_saida-f_transporte IS INITIAL OR wa_saida-pc_partiner IS INITIAL OR wa_saida-lr_partiner IS INITIAL.
            IF p_modo = '1'.
              MESSAGE 'Para Operações com Aviso de Recebimento deve ser Preenchido os Parceiros Agente/Coleta/Entrega!' TYPE 'E'.
            ENDIF.
            EXIT.
          ENDIF.
          UPDATE zib_nfe_dist_ter
             SET f_transporte = wa_saida-f_transporte
                 pc_partiner  = wa_saida-pc_partiner
                 lr_partiner  = wa_saida-lr_partiner
           WHERE chave_nfe EQ wa_saida-chave_nfe.
          COMMIT WORK.
        ENDIF.

      ENDIF.

      EXPORT v_bukrs_memo          TO MEMORY ID 'EMPRESA'.
      EXPORT v_branch_memo         TO MEMORY ID 'FILIAL'.
      EXPORT v_numero_memo         TO MEMORY ID 'NFE'.
      EXPORT v_serie_memo          TO MEMORY ID 'SERIE'.
      EXPORT v_dt_emissao_memo     TO MEMORY ID 'DT_EMISSAO'.
      EXPORT v_lifnr_memo          TO MEMORY ID 'IDPARCEIRO'.
      EXPORT v_operacao_memo       TO MEMORY ID 'OPERACAO'.
      EXPORT v_chave_nfe_memo      TO MEMORY ID 'CHAVE'.
      EXPORT v_lcto_memo           TO MEMORY ID 'LANCAMENTO'.
      EXPORT v_agente              TO MEMORY ID 'AGENTE'.
      EXPORT v_coleta              TO MEMORY ID 'COLETA'.
      EXPORT v_entrega             TO MEMORY ID 'ENTREGA'.
      EXPORT v_loc_carrega         TO MEMORY ID 'LOC_CARREGA'. "CS2020001418 - Inicio - CSB
*-CS2021000595 - 22.06.2021 - JT - inicio
      EXPORT v_automatico_memo     TO MEMORY ID 'AUTOMATICO'.
*-CS2021000595 - 22.06.2021 - JT - fim

      CALL TRANSACTION 'ZNFW0002' AND SKIP FIRST SCREEN.

*     PERFORM: selecao_dados.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GERAR_PRE_LANC_WRITER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM gerar_pre_lanc_writer.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow IS INITIAL.
    MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
    EXIT.
  ELSE.
    READ TABLE tg_selectedrow INTO wg_selectedrow INDEX 1.

*-CS2021000595 - 22.06.2021 - JT - inicio
    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
    IF sy-subrc IS INITIAL.
      PERFORM gera_pre_lancamento USING wa_saida
                                        '1'.
    ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_PRE_LANC_automatico
*&---------------------------------------------------------------------*
FORM gerar_pre_lanc_automatico.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  DESCRIBE TABLE tg_selectedrow LINES DATA(l_total).

  IF tg_selectedrow IS INITIAL.
    MESSAGE 'Favor selecione pelo menos uma linha!' TYPE 'S'.
    EXIT.
  ELSE.
    LOOP AT tg_selectedrow INTO wg_selectedrow.

      l_perc = ( sy-tabix / l_total ) * 100.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = l_perc
          text       = 'Gerando Pré-lancamentos...'.

*-CS2021000595 - 22.06.2021 - JT - inicio
      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
      IF sy-subrc IS INITIAL.
        PERFORM gera_pre_lancamento USING wa_saida
                                          '2'.
      ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim
    ENDLOOP.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESSAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM processar_documentos .

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow IS INITIAL.
    MESSAGE 'Favor selecione uma linha!' TYPE 'S'.
    EXIT.
  ELSE.
*-CS2021000595 - 22.06.2021 - JT - inicio
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = TEXT-004
      IMPORTING
        answer         = wl_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    CASE wl_answer.
      WHEN '2' OR 'A'.
        EXIT.
    ENDCASE.
*-CS2021000595 - 22.06.2021 - JT - fim

    LOOP AT tg_selectedrow  INTO wg_selectedrow.

      READ TABLE it_saida INTO wa_saida INDEX  wg_selectedrow-index.

      CHECK sy-subrc EQ 0.

      IF wa_saida-seq_lcto IS INITIAL.
        MESSAGE 'Ação não permitida! Favor gere o pré-Lançamento para processar o(s) documento(s).' TYPE 'E'.
        EXIT.
      ELSEIF  wa_saida-bloqueado IS NOT INITIAL.
        MESSAGE 'Ação não permitida! Não existe lançamento para a chave informada' TYPE 'E'.
        EXIT.
      ELSE.

*-CS2021000595 - 22.06.2021 - JT - inicio
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            text_question  = text-004
*          IMPORTING
*            answer         = wl_answer
*          EXCEPTIONS
*            text_not_found = 1
*            OTHERS         = 2.
*
*        CASE wl_answer.
*          WHEN '2' OR 'A'.
*            EXIT.
*        ENDCASE.
*-CS2021000595 - 22.06.2021 - JT - fim

        IF wa_saida-status(4) NE '@9R@'.
          IF wa_saida-status_proc NE  'P'.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = wa_saida-seq_lcto.

          ENDIF.
          ADD 1  TO wl_cont.
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando aprovação, não serão processados!'.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF wl_cont GT 0.
      IF wg_ativo IS INITIAL.
        CALL METHOD ob_timer->run.
        wg_ativo = 'X'.
      ENDIF.
      MESSAGE s836(sd) WITH 'Os documentos foram marcados para processamento!'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ESTORNAR_DOCUMENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM estornar_documentos .

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = tg_selectedrow.

  IF tg_selectedrow IS INITIAL.
    MESSAGE  'Favor selecione uma linha!' TYPE 'S'.
    EXIT.
  ELSE.
*-CS2021000595 - 22.06.2021 - JT - inicio
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question  = TEXT-005
      IMPORTING
        answer         = wl_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    CASE wl_answer.
      WHEN '2' OR 'A'.
        EXIT.
    ENDCASE.
*-CS2021000595 - 22.06.2021 - JT - fim

    LOOP AT tg_selectedrow INTO wg_selectedrow.

      READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.

      IF wa_saida-seq_lcto IS INITIAL.
        MESSAGE 'Ação não permitida! Não existe o  Seq.Lcto  para realizar o processo de estorno' TYPE 'E'.
        EXIT.
      ELSE.

*-CS2021000595 - 22.06.2021 - JT - inicio
*        CALL FUNCTION 'POPUP_TO_CONFIRM'
*          EXPORTING
*            text_question  = text-005
*          IMPORTING
*            answer         = wl_answer
*          EXCEPTIONS
*            text_not_found = 1
*            OTHERS         = 2.
*
*        CASE wl_answer.
*          WHEN '2' OR 'A'.
*            EXIT.
*        ENDCASE.
*-CS2021000595 - 22.06.2021 - JT - inicio

        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(wl_zsdt0001)
           WHERE seq_lcto EQ @wa_saida-seq_lcto.

        IF sy-subrc = 0.
          IF NOT '11_12' CS wl_zsdt0001-st_proc.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Este lançamento é original da ZLES0115'
                                                    'faça o estorno pelo cockpit'.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF wa_saida-status NE '@9R@'.

          CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = wa_saida-seq_lcto
              i_estorno  = 'X'
            TABLES
              t_docs     = tl_docs.

          ADD 1 TO wl_cont.
        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Os documentos que estão aguardando apro~vação, não serão processados'.
        ENDIF.
      ENDIF.

    ENDLOOP.
    IF wl_cont GT 0.
      IF wg_ativo IS INITIAL.
        CALL METHOD ob_timer->run.
        wg_ativo = 'X'.
      ENDIF.
      MESSAGE s836(sd) WITH 'Os documentos foram marcados para estorno!'.
    ENDIF.

  ENDIF.
ENDFORM.

FORM chama_log_bapi  TABLES   tl_return STRUCTURE zfiwrt1000.
  DATA: BEGIN OF tl_log_bapi OCCURS 0,
          tipo      TYPE c,
          line(255) TYPE c,
        END OF tl_log_bapi.

  DATA: wl_layout TYPE slis_layout_alv.

  LOOP AT tl_return.
    MOVE: tl_return-type     TO tl_log_bapi-tipo,
          tl_return-mensagem TO tl_log_bapi-line.

    APPEND tl_log_bapi.
  ENDLOOP.

  REFRESH: estrutura.

  PERFORM montar_layout_log.

  wl_layout-zebra = abap_true.
  wl_layout-colwidth_optimize = abap_true.

  IF tl_log_bapi[] IS NOT INITIAL.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program    = sy-repid
        it_fieldcat           = estrutura[]
        is_layout             = wl_layout
        i_save                = 'A'
        i_screen_start_column = 10
        i_screen_start_line   = 3
        i_screen_end_column   = 80
        i_screen_end_line     = 20
      TABLES
        t_outtab              = tl_log_bapi.

  ENDIF.
ENDFORM.                    " CHAMA_LOG_BAPI

FORM montar_layout_log.
  PERFORM montar_estrutura_log USING:
        1 ' '  ' ' 'TL_LOG_BAPI' 'TIPO'  'Tipo de Msg.'  ' ',
        2 ' '  ' ' 'TL_LOG_BAPI' 'LINE'  'Log. de Exec.'  ' ' .


ENDFORM.                    " MONTAR_LAYOUT

FORM montar_estrutura_log USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  FM_MOVE_VALUES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LIT_ROWS  text
*----------------------------------------------------------------------*
FORM fm_move_values TABLES p_et_index_rows
                        STRUCTURE lvc_s_row.

  DATA: lwa_selected_line LIKE lvc_s_row,
        lf_row_index      TYPE lvc_index,
        lva_loc_carrega   TYPE zfiwrt0025-loc_carrega,
        lva_operacao      TYPE zfiwrt0025-operacao,
        lva_destino_cnpj  TYPE lfa1-stcd1,
        lva_destino_ie    TYPE zib_nfe_dist_ter-destino_ie,
        lva_destino_lifnr TYPE lfa1-lifnr,
        lva_destino_name1 TYPE lfa1-name1,
        lva_f_transporte  TYPE zib_nfe_dist_ter-f_transporte,
        lva_pc_partiner   TYPE zib_nfe_dist_ter-pc_partiner,
        lva_lr_partiner   TYPE zib_nfe_dist_ter-lr_partiner.



  LOOP AT p_et_index_rows INTO lwa_selected_line.

    lf_row_index = lwa_selected_line-index.

    READ TABLE it_saida INDEX lf_row_index INTO wa_saida .
    IF wa_saida-loc_carrega IS NOT INITIAL.
      lva_loc_carrega    = wa_saida-loc_carrega.
*      lva_operacao       = wa_saida-operacao.
*      lva_destino_cnpj   = wa_saida-destino_cnpj.
*      lva_destino_ie     = wa_saida-destino_ie.
*      lva_destino_lifnr  = wa_saida-destino_lifnr.
*      lva_destino_name1  = wa_saida-destino_name1.
*      lva_f_transporte   = wa_saida-f_transporte.
*      lva_lr_partiner    = wa_saida-lr_partiner.
*      lva_pc_partiner    = wa_saida-pc_partiner.
      EXIT.
    ENDIF.
  ENDLOOP.

  LOOP AT p_et_index_rows INTO lwa_selected_line.

    lf_row_index = lwa_selected_line-index.

    READ TABLE it_saida INDEX lf_row_index INTO wa_saida .
    IF wa_saida-loc_carrega IS INITIAL.

      wa_saida-loc_carrega     =  lva_loc_carrega.
      SELECT SINGLE *
        FROM zfiwrt0025 INTO @DATA(wa_zfiwrt0025)
        WHERE bukrs       EQ @wa_saida-bukrs
        AND   loc_carrega EQ @lva_loc_carrega
        AND   cfop        EQ @wa_saida-prod_cfop.

      IF sy-subrc = 0.
        PERFORM  check_av_recebimento USING wa_zfiwrt0025-operacao
                                            wa_saida.
        wa_saida-operacao = wa_zfiwrt0025-operacao.
*        wa_saida-operacao        =  lva_operacao.
*        wa_saida-destino_cnpj    =  lva_destino_cnpj.
*        wa_saida-destino_ie      =  lva_destino_ie.
*        wa_saida-destino_lifnr   =  lva_destino_lifnr.
*        wa_saida-destino_name1   =  lva_destino_name1.
*        wa_saida-f_transporte    =  lva_f_transporte.
*        wa_saida-lr_partiner     =  lva_lr_partiner.
*        wa_saida-pc_partiner     =  lva_pc_partiner.

        MODIFY it_saida FROM wa_saida INDEX lf_row_index .
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_AV_RECEBIMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_OPERACAO  text
*----------------------------------------------------------------------*
FORM check_av_recebimento  USING p_operacao
                                 p_saida  TYPE ty_saida.

  DATA: lva_numero TYPE  zib_nfe_dist_ter-numero.

  SELECT * INTO TABLE @DATA(it_zfiwrt0001)
    FROM zfiwrt0001
  WHERE operacao EQ @p_operacao.

  SORT it_zfiwrt0001 BY operacao.
  DELETE ADJACENT DUPLICATES FROM it_zfiwrt0001 COMPARING operacao.

  IF it_zfiwrt0001[] IS NOT INITIAL.

    SELECT *
    FROM j_1bad INTO TABLE @DATA(lit_j1bad)
    FOR ALL ENTRIES IN @it_zfiwrt0001
    WHERE parvw EQ @it_zfiwrt0001-parvw.


    READ TABLE it_zfiwrt0001 WITH KEY operacao = p_operacao BINARY SEARCH INTO DATA(wa_zfiwrt0001).
    IF sy-subrc IS INITIAL AND wa_zfiwrt0001-aviso_rec EQ 'S'.
      IF wa_zfi0024 IS NOT INITIAL.
        p_saida-f_transporte = wa_zfi0024-cod_sb.
        p_saida-pc_partiner  = wa_zfi0024-cod_pc.
        p_saida-lr_partiner  = wa_zfi0024-cod_lr.
      ENDIF.

      CLEAR: p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_enabled ) TO p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_enabled ) TO p_saida-cellstyles.

    ELSE.
      CLEAR: p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'F_TRANSPORTE' style = cl_gui_alv_grid=>mc_style_disabled ) TO p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'LR_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO p_saida-cellstyles.
      APPEND VALUE #( fieldname = 'PC_PARTINER'  style = cl_gui_alv_grid=>mc_style_disabled ) TO p_saida-cellstyles.
    ENDIF.

    CLEAR:lva_numero.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = p_saida-numero
      IMPORTING
        output = lva_numero.


    READ TABLE it_zib_nfe_dist_ter INTO DATA(wa_zib_nfe_dist_ter_aux) WITH KEY bukrs  = p_saida-bukrs
                                                                               branch = p_saida-branch
                                                                               numero = lva_numero
                                                                               serie  = p_saida-serie.
    IF sy-subrc <> 0.
      READ TABLE it_zib_nfe_dist_ter INTO wa_zib_nfe_dist_ter_aux  WITH KEY bukrs_e  = p_saida-bukrs
                                                                            branch_e = p_saida-branch
                                                                            numero = lva_numero
                                                                            serie  = p_saida-serie.
    ENDIF.

    READ TABLE it_zfiwrt0001 INTO DATA(lwa_zfit0001) WITH KEY operacao = p_operacao.
    IF sy-subrc EQ 0.
      READ TABLE lit_j1bad INTO DATA(lwa_j_1bad) WITH KEY parvw = lwa_zfit0001-parvw.
      IF sy-subrc EQ 0.
        CASE lwa_j_1bad-partyp.
          WHEN 'C'.
            READ TABLE tg_kna1 INTO wg_kna1 WITH KEY stcd1 = wa_zib_nfe_dist_ter_aux-destino_cnpj
                                                     stcd3 = wa_zib_nfe_dist_ter_aux-destino_ie.
            IF sy-subrc = 0.
              p_saida-destino_cnpj  = wg_kna1-stcd1.
              p_saida-destino_ie    = wg_kna1-stcd3.
              p_saida-destino_lifnr = wg_kna1-kunnr.
              p_saida-destino_name1 = wg_kna1-name1.
            ENDIF.
          WHEN 'V'.
            READ TABLE tg_lfa1 INTO wg_lfa1 WITH KEY stcd1 = wa_zib_nfe_dist_ter_aux-destino_cnpj
                                                     stcd3 = wa_zib_nfe_dist_ter_aux-destino_ie.
            IF sy-subrc = 0.
              p_saida-destino_cnpj  = wg_lfa1-stcd1.
              p_saida-destino_ie    = wg_lfa1-stcd3.
              p_saida-destino_lifnr = wg_lfa1-lifnr.
              p_saida-destino_name1 = wg_lfa1-name1.
            ENDIF.
        ENDCASE.
      ENDIF.
    ENDIF.

    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd1 = wa_zib_nfe_dist_ter_aux-forne_cnpj
                                             stcd3 = wa_zib_nfe_dist_ter_aux-forne_ie.
    IF sy-subrc = 0.
      p_saida-lifnr    = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      p_saida-name1    = wa_lfa1-name1.

      SELECT SINGLE *
        FROM j_1bbranch INTO @DATA(wa_j_1bbranch)
       WHERE branch EQ @wa_lfa1-lifnr+6(4).

      IF sy-subrc EQ 0.
        p_saida-pbukrs  = wa_j_1bbranch-bukrs.
        p_saida-pbranch = wa_j_1bbranch-branch.
      ENDIF.
    ENDIF.

*-CS2021000595 - 22.06.2021 - JT - inicio
    READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY stcd2 = wa_zib_nfe_dist_ter_aux-forne_cpf
                                             stcd3 = wa_zib_nfe_dist_ter_aux-forne_ie.
    IF sy-subrc = 0.
      p_saida-lifnr    = |{ wa_lfa1-lifnr ALPHA = OUT }|.
      p_saida-name1    = wa_lfa1-name1.

      SELECT SINGLE *
        FROM j_1bbranch INTO wa_j_1bbranch
       WHERE branch EQ wa_lfa1-lifnr+6(4).

      IF sy-subrc EQ 0.
        p_saida-pbukrs  = wa_j_1bbranch-bukrs.
        p_saida-pbranch = wa_j_1bbranch-branch.
      ENDIF.
    ENDIF.
  ENDIF.
*-CS2021000595 - 22.06.2021 - JT - inicio

ENDFORM.
