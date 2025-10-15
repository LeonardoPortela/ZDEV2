*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F30
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SET_NFE_NUMBER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM set_nfe_number .

  DATA: vl_guia_agro         TYPE c,         "<<<------"188425 - NMS ------->>>
        vl_msg_numguia_usada TYPE char128.   "<<<------"188425 - NMS ------->>>

  DATA: wa_znfecomex      TYPE znfecomex,
        it_itens          TYPE TABLE OF j_1bnflin INITIAL SIZE 0,
        wa_itens          TYPE j_1bnflin,
        it_stx            TYPE TABLE OF j_1bnfstx, "EQUALIZAÇÃO ECC X HANA - SMC
        it_j_1batl1       TYPE TABLE OF j_1batl1, "EQUALIZAÇÃO ECC X HANA - SMC
        vl_icms_partilha  TYPE c,
        wl_zsdt0128       TYPE zsdt0128,
        vl_aux(2)         TYPE c,
        vl_user_form_lote TYPE tvarvc-low.


* Check authorization
  IF gf_authorization_nfe_35 IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '056'.
  ENDIF.

* Check if an NF-e selection was made
  IF it_selected_rows IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '030'.
    RETURN.
  ENDIF.

* Ckear the work area
  CALL FUNCTION 'J_1B_NFE_REFRESH_ACTIVE'.  "1689497

* Send NF-e that was posted under contingency
  CLEAR subrc.
  REFRESH it_active_mod.
  LOOP AT it_selected_rows INTO wa_selected_rows.

    READ TABLE it_nfe_alv INTO wa_nfe_alv INDEX wa_selected_rows-index.
* process numbering only for RFC call types 1 and 2

    CLEAR: subrc.
    "Percorrer itens da notafiscal
    "Se CFOP(1) eq '7', verificar se existe dados de exportação
    "se não existe dar mensagem de erro

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
    TRY.
        zcl_doc_eletronico=>validacao_autorizacao_uso( i_docnum =  wa_nfe_alv-docnum ).
      CATCH zcx_doc_eletronico INTO DATA(lcx_doc_eletronico). " Classe de Erro de Documento Eletrônico
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        subrc = c_x.
    ENDTRY.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

    PERFORM f_sinc_documento_sap_ecc USING wa_nfe_alv-docnum 0 CHANGING subrc.

    CHECK subrc EQ 0.

    IF ( sy-tcode EQ 'ZCTE' ) OR ( sy-tcode EQ 'ZNFE' ).

      PERFORM verifica_valor_registro USING wa_nfe_alv-docnum
                                   CHANGING subrc.

      IF subrc IS NOT INITIAL.
        MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER 023
           WITH 'Valor do documento ultrapassa o limite permitido'
                'e necessita de uma aprovação do Departamento'
                'Fiscal. Favor criar uma FI ao indiretos no Soft '
                'Expert solicitando a validação deste documento'.
      ENDIF.
    ENDIF.

    IF sy-tcode EQ 'ZCTE'.
      CALL METHOD zcl_repom_viagem_vpr=>verifica_custo_vi
        EXPORTING
          i_docnum_cte = wa_nfe_alv-docnum
        EXCEPTIONS
          custo_vi     = 1
          OTHERS       = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    IF sy-tcode EQ 'ZCTE'.
      DATA: lit_zcte_info_nota_proc TYPE TABLE OF zcte_info_nota.

      CLEAR: lit_zcte_info_nota_proc[].

      SELECT *
        FROM zcte_info_nota INTO TABLE lit_zcte_info_nota_proc
       WHERE docnum EQ wa_nfe_alv-docnum.

      LOOP AT lit_zcte_info_nota_proc INTO DATA(lwa_zcte_info_nota_proc) WHERE docnum_nf IS NOT INITIAL.

        SELECT SINGLE *
          FROM j_1bnfe_active INTO @DATA(lwa_active_nfe_cte)
         WHERE docnum EQ @lwa_zcte_info_nota_proc-docnum_nf.

        IF ( sy-subrc EQ 0 ) AND ( lwa_active_nfe_cte-docsta EQ '1' ) AND ( lwa_active_nfe_cte-action_requ IS INITIAL ).
          subrc = c_x.
          MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER 023
             WITH |Documento  { lwa_zcte_info_nota_proc-docnum_nf } em processamento|
                  'na transação ZNFE.'
                  'Operação não permitida!'.
        ENDIF.
      ENDLOOP.
    ENDIF.


    IF sy-tcode EQ 'ZNFE'.

      SELECT * INTO TABLE it_itens
        FROM j_1bnflin
       WHERE docnum EQ wa_nfe_alv-docnum.

      READ TABLE it_itens INDEX 1 INTO wa_itens.

      IF ( ( sy-subrc IS INITIAL ) AND ( wa_itens-cfop(1) EQ '7' ) ).

        CALL FUNCTION 'Z_SD_INFO_NFE_EXPORTACAO'
          EXPORTING
            p_docnum       = wa_nfe_alv-docnum
            p_tela         = space
            p_znfecomex    = wa_znfecomex
          EXCEPTIONS
            nao_localizado = 1
            OTHERS         = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          subrc = c_x.
        ENDIF.

        "Verificar Vinculo Remessa ZSDT0066
        DATA(lva_not_vinc_zsdt0066) = abap_false.
        PERFORM f_verifica_vinc_rem_zsdt0066 USING wa_itens
                                          CHANGING lva_not_vinc_zsdt0066.

        IF lva_not_vinc_zsdt0066 EQ abap_true.
          subrc = c_x.
        ENDIF.

      ENDIF.

      CALL FUNCTION 'ZPLANCOMP_REC_DEV_EXPORTACAO'
        EXPORTING
          p_docnum_rec       = wa_nfe_alv-docnum
        EXCEPTIONS
          cancelado          = 1
          nota_fiscal_compro = 2
          OTHERS             = 3.

      IF ( sy-subrc EQ 1 ) OR ( sy-subrc EQ 2 ).
        MESSAGE ID sy-msgid TYPE 'W' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        subrc = c_x.
      ENDIF.

      CLEAR: vl_icms_partilha.
      CALL FUNCTION 'ZCALC_ICMS_VENDA_INTERESTADUAL'
        EXPORTING
          i_docnum    = wa_itens-docnum
          i_itmnum    = wa_itens-itmnum
          i_cfop      = wa_itens-cfop
        IMPORTING
          e_calculado = vl_icms_partilha.

      IF vl_icms_partilha IS NOT INITIAL.
        "Verifica se Emissão da NF já foi liberada pelo Depto. Fiscal.
        CLEAR: wl_zsdt0128.
        SELECT SINGLE *
          FROM zsdt0128 INTO wl_zsdt0128
         WHERE docnum = wa_itens-docnum.

        IF sy-subrc NE 0.
          MESSAGE ID 'ZSIMETRYA' TYPE 'W'
                                 NUMBER 023
             WITH 'Essa NF de Venda Interestadual para não'
                  'contribuinte requer aprovação do Departamento'
                  'Fiscal. Favor criar uma FI ao indiretos no Soft '
                  'Expert solicitando a validação deste documento'.

          subrc = c_x.
        ENDIF.
      ENDIF.

      "Bloquear Determinação Numeração Manualmente quando for usuario de Serviço
      IF wa_nfe_alv-crenam EQ 'WSSE_SIGAM'.
        MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER 023
           WITH 'Essa NF foi criada pelo Sistema SIGAM.'
                'Operação não permitida!'.
        subrc = c_x.
      ENDIF.

      "Bloquear Determinação Numeração quando NF-e referenciar mais de 999 documentos
      DATA: lva_count_nfe_ref TYPE i.

      SELECT COUNT( * )
        FROM zsdt_retlote INTO lva_count_nfe_ref
       WHERE docnum_ret EQ wa_nfe_alv-docnum.

      IF lva_count_nfe_ref > 999.
        MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER 023
           WITH 'NF de Retorno referencia mais de 999 documentos.'
                'Operação não permitida!'.
        subrc = c_x.
      ENDIF.

      PERFORM f_ck_geracao_vt_vi_frete_cpt USING wa_nfe_alv-docnum CHANGING subrc.

**<<<------" User Story 151256 - AOENNING - Início------>>>
      "Verificar unidade tributada material.
      SELECT * FROM j_1bnflin INTO TABLE @DATA(it_j_1bnflin)
      WHERE docnum EQ @wa_nfe_alv-docnum.
      IF sy-subrc EQ 0.
        LOOP AT it_j_1bnflin INTO DATA(wa_1bnflin).

          zcl_im_cl_nfe_print=>get_unid_tribut_item_nf(
            EXPORTING
              i_lin       = wa_1bnflin            " Partidas individuais da nota fiscal
            IMPORTING
              e_unid_trib = DATA(vga_gewei)       " Unidade de peso
              e_qtd_trib  = DATA(vga_brgew)       " Peso bruto
              e_msg_error = DATA(vga_msg_error)   " Variável da mensagem 1
          ).

          IF vga_msg_error IS NOT INITIAL.
            MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023 WITH vga_msg_error.
            subrc = c_x.
            EXIT.
          ENDIF.
          CLEAR: vga_gewei, vga_brgew.
        ENDLOOP.
      ENDIF.
**<<<------" User Story 151256 - AOENNING - Fim------>>>!
    ENDIF.

* USER STORY 73154 - MMSILVA - 10.02.2025 - Inicio - ATUALIZAÇÃO: CANCELADO DEVIDO NÃO TER MAIS CASOS E TER IMPACTO NA ZNFW0016, ALINHADO COM ANDERSON OENNING.
*    select *
*       into table it_itens
*       from j_1bnflin
*       where docnum eq wa_nfe_alv-docnum.
*
*    loop at it_itens into wa_itens.
*
*      if wa_itens-netwrt = ''.
*
*        select *
*          from zfit0226
*          into table @data(it_zfit0226)
*          where docnum eq @wa_itens-docnum.
*
*        if sy-subrc ne 0.
*          message id 'ZSIMETRYA' type 'E' number 048.
*        endif.
*
*      endif.
*
*    endloop.
* USER STORY 73154 - MMSILVA - 10.02.2025 - Fim - ATUALIZAÇÃO: CANCELADO DEVIDO NÃO TER MAIS CASOS E TER IMPACTO NA ZNFW0016, ALINHADO COM ANDERSON OENNING.

    "Inicio Alteração - Leandro Valentim Ferreira - 08.09.23 - #115199 - EQUALIZAÇÃO ECC X HANA - SMC
    IF sy-ucomm EQ 'SET_NUM'.
      SELECT * INTO TABLE it_itens
        FROM j_1bnflin
       WHERE docnum EQ wa_nfe_alv-docnum.

      IF sy-subrc EQ 0.
        SELECT * INTO TABLE it_stx
               FROM j_1bnfstx
               FOR ALL ENTRIES IN it_itens
               WHERE docnum EQ it_itens-docnum
                 AND itmnum EQ it_itens-itmnum
                 AND taxtyp EQ 'ICM3'.

        SELECT * INTO TABLE it_j_1batl1
               FROM j_1batl1
               FOR ALL ENTRIES IN it_itens
               WHERE taxlaw EQ it_itens-taxlw1.
      ENDIF.

      LOOP AT it_itens INTO DATA(wl_itens).
        READ TABLE it_j_1batl1 INTO DATA(wl_j_1batl1) WITH KEY taxlaw =  wl_itens-taxlw1.
        IF sy-subrc EQ 0.

          CLEAR vl_aux.
          CALL FUNCTION 'CONVERSION_EXIT_TXSIT_OUTPUT'
            EXPORTING
              input  = wl_j_1batl1-taxsit
            IMPORTING
              output = vl_aux.

          CASE vl_aux.
            WHEN '00'.
              READ TABLE it_stx INTO DATA(wl_stx) WITH KEY docnum = wl_itens-docnum
                                                           itmnum = wl_itens-itmnum.
              IF sy-subrc EQ 0.
                IF wl_stx-base < wl_itens-netwr.
                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                     WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                          'Abra uma FI para o CSC Fiscal!'.
                  subrc = c_x.
                ENDIF.
              ENDIF.
            WHEN '20'.
              READ TABLE it_stx INTO wl_stx WITH KEY docnum = wl_itens-docnum
                                                     itmnum = wl_itens-itmnum.
              IF sy-subrc EQ 0.
                IF wl_stx-base >= wl_itens-netwr OR wl_stx-base IS INITIAL.
                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                     WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                          'Abra uma FI para o CSC Fiscal!'.
                  subrc = c_x.
                ENDIF.
              ENDIF.
            WHEN '40' OR '41' OR '50' OR '51'.
              READ TABLE it_stx INTO wl_stx WITH KEY docnum = wl_itens-docnum
                                                     itmnum = wl_itens-itmnum.
              IF sy-subrc EQ 0.
                IF wl_stx-base > 0.
                  MESSAGE ID 'ZSIMETRYA' TYPE 'E' NUMBER 023
                     WITH 'A BC do ICMS não condiz com o CST da Lei Fiscal.'
                          'Abra uma FI para o CSC Fiscal!'.
                  subrc = c_x.
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
    "*Fim Alteração - Leandro Valentim Ferreira - 08.09.23 - #115199 - EQUALIZAÇÃO ECC X HANA - SMC

    IF subrc IS INITIAL.
      CASE wa_nfe_alv-model.
        WHEN zif_doc_eletronico=>at_st_model_mdfe.

          TRY .
              DATA(obj_mdfe) = NEW zcl_mdfe( i_docnum = wa_nfe_alv-docnum i_nmdfe = wa_nfe_alv-nfnum9 ).
              obj_mdfe->enviar_mdfe( ).
              CLEAR: obj_mdfe.

              SELECT SINGLE * INTO wa_active_mod
                FROM j_1bnfe_active
               WHERE docnum EQ wa_nfe_alv-docnum.

              DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
              APPEND wa_active_mod TO it_active_mod.        "1090279
              PERFORM registra_envio_data USING wa_nfe_alv space.

              gf_docnum = wa_nfe_alv-docnum.

            CATCH cx_root INTO DATA(ex_root).
              CLEAR: obj_mdfe.

              SELECT SINGLE * INTO wa_active_mod
                FROM j_1bnfe_active
               WHERE docnum EQ wa_nfe_alv-docnum.

              DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
              APPEND wa_active_mod TO it_active_mod.        "1090279
              PERFORM registra_envio_data USING wa_nfe_alv space.

              gf_docnum = wa_nfe_alv-docnum.

          ENDTRY.

          "CT-e 4.0 - WPP - INICIO --->> EQUALIZAÇÃO ECC X HANA - SMC  - Case When 57 não deve ir para PRD
*        WHEN '57'.
*
*          SELECT SINGLE *
*            FROM j_1bnfdoc INTO @DATA(lwa_doc_cte)
*          WHERE docnum = @wa_nfe_alv-docnum.
*
*
**          IF sy-subrc EQ 0 AND lwa_doc_cte-xmlvers = '4.00'.
**            CALL FUNCTION 'ZRSI_CTE_400_SET_NUMBER'
**              EXPORTING
**                iv_docnum                 = wa_nfe_alv-docnum
**              IMPORTING
**                es_active_mod             = wa_active_mod
**              EXCEPTIONS
**                numbering_not_possible    = 1
**                lock_error_nfdoc          = 2
**                lock_error_active         = 3
**                already_numbered          = 4
**                rfc_failure               = 5                     "1723346
**                lock_error_nfdoc_for_rfc  = 6                     "1723346
**                lock_error_active_for_rfc = 7                     "1723346
**                OTHERS                    = 8.                    "1723346
***          ELSE.
*          CALL FUNCTION 'J_1B_NFE_SET_NUMBER'
*            EXPORTING
*              iv_docnum              = wa_nfe_alv-docnum
*            IMPORTING
*              es_active_mod          = wa_active_mod
*            EXCEPTIONS
*              numbering_not_possible = 1
*              lock_error_nfdoc       = 2
*              lock_error_active      = 3
*              already_numbered       = 4
*              OTHERS                 = 5.
**         ENDIF.
*
*
*          IF sy-subrc <> 0.
*            subrc = c_x.
*          ELSE.
*
*            PERFORM complementa_zlest0194.
*
*            DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
*            APPEND wa_active_mod TO it_active_mod.
*            PERFORM registra_envio_data USING wa_nfe_alv space.
*          ENDIF.
          "CT-e 4.0 - WPP - FIM>> ---> EQUALIZAÇÃO ECC X HANA - SMC

        WHEN OTHERS.

          DATA: lva_tentativas_envio TYPE i.

          SELECT SINGLE *
            FROM tvarvc INTO @DATA(lwa_tvarvc_tent_envio)
           WHERE name = 'TENTATIVAS_DET_NUM_NFE'.

          IF sy-subrc EQ 0 AND lwa_tvarvc_tent_envio-low IS NOT INITIAL.
            lva_tentativas_envio = lwa_tvarvc_tent_envio-low.
          ELSE.
            lva_tentativas_envio = 1.
          ENDIF.

          DATA(_sy_subrc_set_number) = 9.
          DO lva_tentativas_envio TIMES.

            DATA(_index) = sy-index.

            IF _index NE 1.
              CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
                EXPORTING
                  text = |Determinando numeração do documento fiscal... Tentativa { CONV i( _index ) }|.
            ENDIF.

            CALL FUNCTION 'J_1B_NFE_SET_NUMBER'
              EXPORTING
                iv_docnum              = wa_nfe_alv-docnum
              IMPORTING
                es_active_mod          = wa_active_mod
              EXCEPTIONS
                numbering_not_possible = 1
                lock_error_nfdoc       = 2
                lock_error_active      = 3
                already_numbered       = 4
                OTHERS                 = 5.

            _sy_subrc_set_number = sy-subrc.

            IF _sy_subrc_set_number EQ 1.
              WAIT UP TO 3 SECONDS.
            ELSE.
              EXIT.
            ENDIF.

          ENDDO.

          IF _sy_subrc_set_number <> 0.
            subrc = c_x.
          ELSE.

            PERFORM complementa_zlest0194.

            DELETE it_active_mod WHERE docnum EQ wa_active_mod-docnum.
            APPEND wa_active_mod TO it_active_mod.
            PERFORM registra_envio_data USING wa_nfe_alv space.
          ENDIF.

      ENDCASE.

    ENDIF.

  ENDLOOP.

* clear global tables for server determination in FUGR J_1B_NFE
  CALL FUNCTION 'J_1B_NFE_RESET_AUTO_SERVER'. "1394582
* Update ALV display
  PERFORM grid_update USING space.

  IF NOT subrc IS INITIAL.
    MESSAGE ID 'J1B_NFE' TYPE 'E' NUMBER '024'.
  ELSE.
    MESSAGE ID 'J1B_NFE' TYPE 'S' NUMBER '027'.
  ENDIF.

ENDFORM.                    " SEND_AND_NUMBER_NFE

FORM f_verifica_vinc_rem_zsdt0066  USING p_itens TYPE j_1bnflin
                                CHANGING p_not_vinc_zsdt0066.

  CLEAR: p_not_vinc_zsdt0066.

  CHECK p_itens-reftyp EQ 'BI'.

  SELECT SINGLE *
    FROM vbrp INTO @DATA(lwa_vbrp_exp)
   WHERE vbeln EQ @p_itens-refkey.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM lips INTO @DATA(lwa_lips_exp)
   WHERE vbeln EQ @lwa_vbrp_exp-vgbel.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM vbak INTO @DATA(lwa_vbak_exp)
   WHERE vbeln EQ @lwa_lips_exp-vgbel.

  CHECK ( sy-subrc EQ 0 ).

  CHECK ( lwa_vbak_exp-auart EQ 'ZEXP' ) OR (  lwa_vbak_exp-auart EQ 'ZEXI' ).

  SELECT SINGLE *
    FROM zsdt0053 INTO @DATA(lwa_0053_exp)
   WHERE remessa_exp EQ @lwa_lips_exp-vbeln.

  CHECK sy-subrc EQ 0.

  SELECT SINGLE *
    FROM zdoc_exp INTO @DATA(lwa_zdoc_exp)
   WHERE vbeln EQ @lwa_lips_exp-vbeln.

  IF sy-subrc NE 0.
    p_not_vinc_zsdt0066 = abap_true.

    MESSAGE ID 'ZSIMETRYA' TYPE 'W' NUMBER 023
          WITH 'Não foi realizado o vinculo da remessa'
               'de exportação na ZSDT0066'.
  ENDIF.


ENDFORM.
