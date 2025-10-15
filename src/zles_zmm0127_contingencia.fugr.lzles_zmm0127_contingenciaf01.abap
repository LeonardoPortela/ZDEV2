*----------------------------------------------------------------------*
***INCLUDE LZLES_ZMM0127_CONTINGENCIAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_selecao_hana
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_verifica_duplicidade.

  FREE: t_alv.

  LOOP AT t_zsdt0231_ecc INTO w_zsdt0231_ecc.
    "READ TABLE t_j_1bnfdoc_hana INTO w_j_1bnfdoc_hana WITH KEY docnum = w_zsdt0231_ecc-docnum.
    READ TABLE t_ztnf_hana ASSIGNING FIELD-SYMBOL(<fs_doc_hana>)
      WITH KEY docnum_ecc = w_zsdt0231_ecc-docnum.

    CHECK sy-subrc = 0.

    APPEND w_zsdt0231_ecc  TO t_alv.
  ENDLOOP.

  IF t_alv[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não há duplicidade de registros!'.
    EXIT.
  ENDIF.

  l_program                  = sy-repid.
  l_grid_title               = 'Registros que ja Existem no HANA'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_program
      i_structure_name       = 'ZSDT0231'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program    = l_program
      is_layout             = w_layout
      it_fieldcat           = t_fieldcat
      i_grid_title          = l_grid_title
      i_screen_start_column = 10
      i_screen_start_line   = 02
      i_screen_end_column   = 182
      i_screen_end_line     = 20
    TABLES
      t_outtab              = t_alv
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.

ENDFORM.

FORM f_selecao_hana.

  CHECK  t_zsdt0231_ecc[] IS NOT INITIAL.

  SELECT * FROM ztnf_hana
    INTO TABLE t_ztnf_hana
        FOR ALL ENTRIES IN t_zsdt0231_ecc
     WHERE docnum_ecc = t_zsdt0231_ecc-docnum.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_j_1bnfdoc_hana
     FOR ALL ENTRIES IN t_zsdt0231_ecc
   WHERE docnum = t_zsdt0231_ecc-docnum.

  SELECT *
    FROM zsdt0231
    INTO TABLE t_zsdt0231_hana
     FOR ALL ENTRIES IN t_zsdt0231_ecc
   WHERE docnum = t_zsdt0231_ecc-docnum.

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE t_j_1bnfe_active_hana
     FOR ALL ENTRIES IN t_zsdt0231_ecc
   WHERE docnum = t_zsdt0231_ecc-docnum.

  SELECT *
    FROM zib_nfe
    INTO TABLE t_zib_nfe_hana
     FOR ALL ENTRIES IN t_zsdt0231_ecc
   WHERE docnum = t_zsdt0231_ecc-docnum.

ENDFORM.

FORM f_carrega_notas.

  LOOP AT t_zsdt0231_ecc INTO w_zsdt0231_ecc.
    lc_reg_criado = abap_off.

    "READ TABLE t_j_1bnfdoc_hana INTO w_j_1bnfdoc_hana WITH KEY docnum = w_zsdt0231_ecc-docnum.

    READ TABLE t_ztnf_hana ASSIGNING FIELD-SYMBOL(<fs_doc_hana>)
      WITH KEY docnum_ecc = w_zsdt0231_ecc-docnum.

    CHECK sy-subrc <> 0.

    "PERFORM f_monta_dados             USING lc_dados.


    TRY.

        PERFORM f_criar_nf USING w_zsdt0231_ecc-docnum.

*        zcl_integracao_grc_new_nfe=>zif_integracao_grc_new_nfe~get_instance(
*           )->set_new_doc_discal( EXPORTING i_dados               = lc_dados
*                                  IMPORTING e_documento           = w_j_1bnfdoc_hana
*                                            e_info_doc_eletronico = w_j_1bnfe_active_hana ).
*
*        lc_reg_criado = abap_true.

      CATCH zcx_integracao INTO DATA(ex_integracao).
        DATA(ms_erro) = ex_integracao->zif_error~get_msg_erro( ).
      CATCH zcx_error INTO DATA(ex_erro).
        ms_erro = ex_erro->zif_error~get_msg_erro( ).
      CATCH cx_root INTO DATA(ex_root).
        ms_erro = ex_root->get_text( ).
    ENDTRY.

  ENDLOOP.
*atualizar zsdt0231, j_1nbnfe_Active, zib_nfe.


ENDFORM.

FORM f_monta_dados USING p_dados.

  FREE: p_dados.

  READ TABLE t_j_1bnfdoc_ecc INTO w_j_1bnfdoc_ecc WITH KEY docnum = w_zsdt0231_ecc-docnum.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CRIAR_NF
*&---------------------------------------------------------------------*
FORM f_criar_nf USING uv_docnum TYPE j_1bdocnum.

  DATA fiscal TYPE REF TO zcl_fiscal.

  DATA lt_0231 TYPE TABLE OF zsdt0231.
  DATA lt_zbinfe TYPE TABLE OF zib_nfe.
  DATA ls_de_para TYPE ztnf_hana.

  DATA i_cabecalho TYPE zde_fiscal_cabecalho. "
  DATA i_itens  TYPE zde_fiscal_itens_t. "
  DATA i_impostos	TYPE zde_fiscal_impostos_t. "
  DATA i_parceiros  TYPE zde_fiscal_parceiros_t. "
  DATA i_bapi_wait  TYPE bapiwait VALUE 'X'.
  DATA i_obs_fiscal	TYPE string.
  DATA i_obs_contribuinte	TYPE string.
  DATA e_docnum	TYPE j_1bdocnum.
  DATA e_retorno  TYPE bapiret2_t.
  DATA r_gerou TYPE char01.

  LOOP AT t_j_1bnfdoc_ecc ASSIGNING FIELD-SYMBOL(<fs_doc>) WHERE docnum = uv_docnum.

    CLEAR:  i_cabecalho,
            i_itens,
            i_impostos,
            i_parceiros,
            i_obs_fiscal,
            i_obs_contribuinte,
            e_docnum,
            e_retorno,
            r_gerou,
            lt_0231,
            lt_zbinfe,
            ls_de_para.

    READ TABLE t_zsdt0231_ecc ASSIGNING FIELD-SYMBOL(<fs_231>)
      WITH KEY docnum = uv_docnum.

    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING <fs_doc> TO i_cabecalho.

    CLEAR i_cabecalho-docstat.

    LOOP AT t_j_1bnflin_ecc ASSIGNING FIELD-SYMBOL(<fs_lin>) WHERE docnum = uv_docnum.

      APPEND INITIAL LINE TO i_itens ASSIGNING FIELD-SYMBOL(<fs_item>).
      MOVE-CORRESPONDING <fs_lin> TO <fs_item>.

    ENDLOOP.

    LOOP AT t_j_1bnfnad_ecc ASSIGNING FIELD-SYMBOL(<fs_nad>) WHERE docnum = uv_docnum.

      APPEND INITIAL LINE TO i_parceiros ASSIGNING FIELD-SYMBOL(<fs_parc>).
      MOVE-CORRESPONDING <fs_nad> TO <fs_parc>.

    ENDLOOP.

    LOOP AT t_j_1bnfstx_ecc ASSIGNING FIELD-SYMBOL(<fs_stx>) WHERE docnum = uv_docnum.

      APPEND INITIAL LINE TO i_impostos ASSIGNING FIELD-SYMBOL(<fs_imp>).
      MOVE-CORRESPONDING <fs_stx> TO <fs_imp>.

    ENDLOOP.

    LOOP AT t_j_1bnfftx_ecc ASSIGNING FIELD-SYMBOL(<fs_ftx>) WHERE docnum = uv_docnum.
      i_obs_fiscal = i_obs_fiscal && ``  && <fs_ftx>-message.
    ENDLOOP.

    CREATE OBJECT fiscal.

    fiscal->criar(
      EXPORTING
        i_cabecalho        = i_cabecalho    " Esturtura de transferência dados cabeçalho nota fiscal
        i_itens            = i_itens    " Tabela Estrutura de Itens p/ BAPI Fiscal
        i_impostos         = i_impostos    " Tabela Estrutura de Impostos para BAPI Fiscal
        i_parceiros        = i_parceiros    " Estrutura para parceiros p/ BAPI Fiscal
        i_obs_fiscal       = i_obs_fiscal
        "i_obs_contribuinte = i_dados-processo-intgnfe-infadic-infcpl
        i_bapi_wait        = abap_true
      IMPORTING
        e_docnum         = e_docnum     " Nº documento
        e_retorno        = e_retorno    " Tabela de retorno
      RECEIVING
        r_gerou          = r_gerou   " Campo de texto do comprimento 1
      EXCEPTIONS
        data_fi_mm_nao   = 1
        nao_cte_forn     = 2
        documento_existe = 3
        erro             = 4
        OTHERS           = 5 ).

    IF r_gerou = abap_true.
      DO 10 TIMES.
        SELECT SINGLE * FROM j_1bnfe_active
          INTO @DATA(ls_active)
            WHERE docnum = @e_docnum.

        IF sy-subrc NE 0.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.

      ENDDO.

      ls_de_para-docnum_s4 = e_docnum.
      ls_de_para-docnum_ecc = uv_docnum.
      ls_de_para-credat = sy-datum.
      ls_de_para-cretim = sy-uzeit.
      ls_de_para-uname = sy-uname.

      MODIFY ztnf_hana FROM ls_de_para.

      CHECK ls_active IS NOT INITIAL.

      READ TABLE t_j_1bnfe_active_ecc ASSIGNING FIELD-SYMBOL(<fs_active>)
        WITH KEY docnum = uv_docnum.

      CHECK sy-subrc EQ 0.

      MOVE-CORRESPONDING <fs_active> TO ls_active.

      ls_active-docnum = e_docnum.
      "ls_active-authcod = <fs_231>-protocolo.

      MODIFY j_1bnfe_active FROM ls_active.

      SELECT SINGLE * FROM j_1bnfdoc
        INTO @DATA(ls_new_doc)
          WHERE docnum = @e_docnum.

      IF sy-subrc EQ 0.

        ls_new_doc-pstdat  = <fs_doc>-pstdat.
        ls_new_doc-docdat  = <fs_doc>-docdat.
        ls_new_doc-PARVW   = <fs_doc>-PARVW.
        ls_new_doc-PARTYP  = <fs_doc>-PARTYP.
        ls_new_doc-MANUAL  = ABAP_TRUE.

        MODIFY j_1bnfdoc FROM ls_new_doc.

      ENDIF.

      SELECT * FROM j_1bnflin
        INTO TABLE @DATA(lt_new_lin)
          WHERE docnum = @e_docnum.

      LOOP AT lt_new_lin ASSIGNING FIELD-SYMBOL(<fs_new_lin>).

        READ TABLE t_j_1bnflin_ecc ASSIGNING <fs_lin>
          WITH KEY docnum = uv_docnum
                   itmnum = <fs_new_lin>-itmnum.

        CHECK sy-subrc EQ 0.

        <fs_new_lin>-cfop = <fs_lin>-cfop.
        CLEAR: <fs_new_lin>-refkey , <fs_new_lin>-reftyp, <fs_new_lin>-refitm.

      ENDLOOP.

      IF 1 = 2.
        LOOP AT t_zsdt0231_ecc ASSIGNING <fs_231> WHERE docnum = uv_docnum.

          APPEND INITIAL LINE TO lt_0231 ASSIGNING FIELD-SYMBOL(<fs_new_0231>).
          MOVE-CORRESPONDING <fs_231> TO <fs_new_0231>.
          <fs_new_0231>-docnum = e_docnum.

        ENDLOOP.
      ENDIF.

      LOOP AT t_zib_nfe_ecc ASSIGNING FIELD-SYMBOL(<fs_zib_nfe>) WHERE docnum = uv_docnum.

        APPEND INITIAL LINE TO lt_zbinfe ASSIGNING FIELD-SYMBOL(<fs_new_zib_nfe>).
        MOVE-CORRESPONDING <fs_zib_nfe> TO <fs_new_zib_nfe>.

        <fs_new_zib_nfe>-docnum = e_docnum.

        IF <fs_active>-SCSSTA NE '2'.
          <fs_new_zib_nfe>-ds_url_danfe = 'http://vhacxprdci.sap.aroeira.corp:8000/custom/docfiscal/getnfepdf?sap-client=300&i_docnum=' && e_docnum.
        ENDIF.

      ENDLOOP.

      IF lt_new_lin IS NOT INITIAL.
        MODIFY j_1bnflin FROM TABLE lt_new_lin.
      ENDIF.

      IF lt_0231 IS NOT INITIAL.
        MODIFY zsdt0231 FROM TABLE lt_0231.
      ENDIF.

      IF lt_zbinfe IS NOT INITIAL.
        MODIFY zib_nfe FROM TABLE lt_zbinfe.
      ENDIF.

      COMMIT WORK AND WAIT.

    ENDIF.

  ENDLOOP.

ENDFORM.
