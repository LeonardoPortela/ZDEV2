*&---------------------------------------------------------------------*
*& Report  ZSDY0001
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdy0001.


*&---------------------------------------------------------------------*
*& TABLES
*&---------------------------------------------------------------------*

TABLES: zsdyt0049, zsdyt0050.

*&---------------------------------------------------------------------*
*& TYPES
*&---------------------------------------------------------------------*

TYPES: BEGIN OF ty_zsdyt0049,
         obj_key       TYPE zsdyt0049-obj_key,
         interface     TYPE zsdyt0049-interface,
         kunnr         TYPE zsdyt0049-kunnr,
         bstkd         TYPE zsdyt0049-bstkd,
         werks         TYPE zsdyt0049-werks,
         cod_mvto      TYPE zsdyt0049-cod_mvto,
         tp_movto      TYPE zsdyt0049-tp_movto,
         vl_fatura     TYPE zsdyt0049-vl_fatura,
         rg_atualizado TYPE zsdyt0049-rg_atualizado,
         vbeln         TYPE zsdyt0049-vbeln,
         doc_fat       TYPE zsdyt0049-doc_fat,
         nfnum         TYPE zsdyt0049-nfnum,
         dif_prc_pos   TYPE zsdyt0049-dif_prc_pos,
         dif_prc_neg   TYPE zsdyt0049-dif_prc_neg,
         desc_ret_pos  TYPE zsdyt0049-desc_ret_pos,
         desc_ret_neg  TYPE zsdyt0049-desc_ret_neg,
       END OF ty_zsdyt0049,

       BEGIN OF ty_zsdyt0050,
         cod_mvto TYPE  zsdyt0050-cod_mvto,
         auart    TYPE  zsdyt0050-auart,
         vkorg    TYPE  zsdyt0050-vkorg,
         vtweg    TYPE  zsdyt0050-vtweg,
         spart    TYPE  zsdyt0050-spart,
         matnr    TYPE  zsdyt0050-matnr,
         waers    TYPE  zsdyt0050-waers,
         taxcode  TYPE  zsdyt0050-taxcode,
         vkaus    TYPE  zsdyt0050-vkaus,
         taxm1    TYPE  zsdyt0050-taxm1,
         tp_movto TYPE  zsdyt0050-tp_movto,
       END OF ty_zsdyt0050.

*&---------------------------------------------------------------------*
*& DATA INTERNAL
*&---------------------------------------------------------------------*

DATA: it_zsdyt0049    TYPE TABLE OF zsdyt0049,
      it_zsdyt0050    TYPE TABLE OF ty_zsdyt0050,
      it_bdc          TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      it_outreturn    TYPE TABLE OF zfie_ret_document,
      it_zob_mensagem TYPE TABLE OF zob_mensagem.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA: wa_zsdyt0049 TYPE zsdyt0049,
      wa_zsdyt0050 TYPE ty_zsdyt0050,
      wa_outreturn TYPE zfie_ret_document,

      wa_return    TYPE bapiret2.

*&---------------------------------------------------------------------*
*& BAPI
*&---------------------------------------------------------------------*
DATA: order_header_in        LIKE bapisdhd1 OCCURS 0 WITH HEADER LINE,
      order_items_in         LIKE bapisditm OCCURS 0 WITH HEADER LINE,

      business_object        LIKE bapiusw01-objtype,
      salesdocument          LIKE bapivbeln-vbeln,

      it_order_conditions_in TYPE TABLE OF bapicond,
      it_order_partners      TYPE TABLE OF bapiparnr,
      it_schedules           TYPE TABLE OF bapischdl,
      it_return              TYPE TABLE OF bapiret2.



DATA:
  wa_order_conditions_in TYPE bapicond,
  wa_order_partners      TYPE bapiparnr,
  wa_schedules           TYPE bapischdl.

DATA: msg     TYPE c LENGTH 255,
      vl_erro TYPE c LENGTH 1.


*&---------------------------------------------------------------------*
*&  START-OF-SELECTION
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM: seleciona_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados.

  DATA: vg_job TYPE i.

  REFRESH: it_zsdyt0049[].


  SELECT SINGLE COUNT(*) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'FATURA_PROPRIA_AR'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ). "and ( raw_enq[] is initial ).

    "--------->>>>>>Busca registros que ja gerou a fatura , porém ainda não atualizou a fatura eletronica.
    REFRESH: it_zsdyt0049[].
    SELECT *
      FROM zsdyt0049
      INTO TABLE it_zsdyt0049
     WHERE rg_atualizado EQ 'C'.

    LOOP AT it_zsdyt0049 INTO wa_zsdyt0049.
      PERFORM verifica_fat_eletronica.
    ENDLOOP.

    "--------->>>>>Criação da ordem e Fatura<<<<<<------------------
    REFRESH: it_zsdyt0049[].

    SELECT *
      FROM zsdyt0049
      INTO TABLE it_zsdyt0049
    WHERE rg_atualizado EQ 'N'.

    SELECT *
      FROM zsdyt0049
      APPENDING TABLE it_zsdyt0049
    WHERE rg_atualizado EQ 'S'
      AND doc_fat       EQ space.

    LOOP AT it_zsdyt0049 INTO wa_zsdyt0049.
*      REFRESH it_outreturn[].
      PERFORM atualiza_reg_lido USING wa_zsdyt0049.
      IF NOT ( wa_zsdyt0049-vbeln IS INITIAL ) AND ( wa_zsdyt0049-doc_fat IS INITIAL )."Caso ja tenha gerado a ov e não tenha gerado a fatura, tenta gerar a fatura novamente.
        PERFORM: criacao_faturamento USING wa_zsdyt0049.
      ELSE.
        vl_erro = 'X'.
        PERFORM: criacao_ov USING wa_zsdyt0049.
        IF vl_erro NE 'X'.
          PERFORM  criacao_faturamento USING wa_zsdyt0049.
        ENDIF.
      ENDIF.
      CLEAR: wa_zsdyt0049, wa_zsdyt0050.
    ENDLOOP.

    "--------->>>>>Criação de PDF da Fatura<<<<<<------------------
    REFRESH: it_zsdyt0049[].
    SELECT *
      FROM zsdyt0049
      INTO TABLE it_zsdyt0049
     WHERE rg_atualizado EQ 'S'
       AND rg_pdf_fatura EQ 'N'.

    LOOP AT it_zsdyt0049 INTO wa_zsdyt0049.
      PERFORM gerar_pdf_fatura USING wa_zsdyt0049.
    ENDLOOP.

    "--------->>>>>Envia Doc CTB da Fatura <<<<<<------------------
    REFRESH: it_zsdyt0049[].
    SELECT *
      FROM zsdyt0049
      INTO TABLE it_zsdyt0049
     WHERE rg_atualizado EQ 'S'
       AND rg_ctb_fatura EQ 'N'.

    LOOP AT it_zsdyt0049 INTO wa_zsdyt0049.
      PERFORM gerar_ctb_fatura USING wa_zsdyt0049.
    ENDLOOP.

    "--------->>>>>Envia as mensagens de erro/sucesso para o Sigam<<<<<<------------------
    PERFORM envia_mensagem_xi.

  ENDIF.

ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  CRIACAO_OV
*&---------------------------------------------------------------------*
FORM criacao_ov USING wa_zsdyt0049 STRUCTURE wa_zsdyt0049.

  DATA: return_commit   LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        return_rollback LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
        vg_value        TYPE WTGXXX.

  CLEAR: salesdocument.

  REFRESH: return_commit[],
           return_rollback[],
           order_header_in,
           order_items_in,
           it_order_conditions_in,
           it_schedules,
           it_order_partners,
           it_return.

  SELECT cod_mvto auart vkorg vtweg spart matnr waers taxcode vkaus taxm1 tp_movto
    FROM zsdyt0050
    INTO TABLE it_zsdyt0050
  WHERE cod_mvto EQ wa_zsdyt0049-cod_mvto
    AND tp_movto EQ wa_zsdyt0049-tp_movto.

  IF ( sy-subrc EQ 0 ).

    READ TABLE it_zsdyt0050 INTO wa_zsdyt0050 WITH KEY cod_mvto = wa_zsdyt0049-cod_mvto  tp_movto = wa_zsdyt0049-tp_movto.

    "Header
    order_header_in-doc_type     =  wa_zsdyt0050-auart.
    order_header_in-sales_org    =  wa_zsdyt0050-vkorg.
    order_header_in-distr_chan   =  wa_zsdyt0050-vtweg.
    order_header_in-division     =  wa_zsdyt0050-spart.
    order_header_in-purch_no_c   =  wa_zsdyt0049-bstkd.
    order_header_in-ord_reason   =  wa_zsdyt0050-vkaus.
    order_header_in-pmnttrms     =  '0001'.
    order_header_in-incoterms1   =  'FOB'.
    order_header_in-incoterms2   =  'Argentina'.
    APPEND order_header_in.

    "Itens
    order_items_in-itm_number   = '000010'.
    order_items_in-material     = wa_zsdyt0050-matnr.
    order_items_in-plant        = wa_zsdyt0049-werks.
    order_items_in-target_qty   = 1.
    order_items_in-gross_wght   = 1.
    order_items_in-net_weight   = 1.
    order_items_in-untof_wght   = 'KG'.
    order_items_in-sd_taxcode   = wa_zsdyt0050-taxcode.
    order_items_in-tax_class1   = wa_zsdyt0050-taxm1.
    APPEND order_items_in.

    " Schedule
    wa_schedules-itm_number = '000010'.
    wa_schedules-req_qty    = 1.
    APPEND wa_schedules TO it_schedules.

    "Conditions

        "Condição YV01
        wa_order_conditions_in-itm_number  =  '000010'.
        wa_order_conditions_in-cond_type   =  'PR00'.
        IF ( wa_zsdyt0049-vr_base_iva > 0 ) .
          wa_order_conditions_in-cond_value  =  wa_zsdyt0049-vr_base_iva.
        ELSE.
          wa_order_conditions_in-cond_value  =  wa_zsdyt0049-vl_fatura.
        ENDIF.

        wa_order_conditions_in-currency    =  wa_zsdyt0050-waers.
        APPEND wa_order_conditions_in TO it_order_conditions_in.
        CLEAR wa_order_conditions_in.


    CASE wa_zsdyt0050-auart. "Ajuste 14/05/2024 / BUG SOLTO 140788 / AOENNING.
      WHEN 'YV21' OR 'YV22'.

        "Condição YV01 - dif_prc_pos
        wa_order_conditions_in-itm_number   =  '000010'.
        wa_order_conditions_in-cond_type    =  'YV01'.

"Ajuste 14/05/2024 / BUG SOLTO 140788 / AOENNING.
*        wa_order_conditions_in-cond_value  =  wa_zsdyt0049-dif_prc_pos.
*        wa_order_conditions_in-currency    =  wa_zsdyt0050-waers.
         wa_order_conditions_in-condvalue   =  wa_zsdyt0049-dif_prc_pos.
         wa_order_conditions_in-CURRENCY_2  =  wa_zsdyt0050-waers.
        if wa_order_conditions_in-condvalue > 0.
        APPEND wa_order_conditions_in TO it_order_conditions_in.
        endif.
        CLEAR wa_order_conditions_in.

        "Condição YV01 - dif_prc_neg
        wa_order_conditions_in-itm_number  =  '000010'.
        wa_order_conditions_in-cond_type   =  'YV01'.

"Ajuste 14/05/2024 / BUG SOLTO 140788 / AOENNING.
*        wa_order_conditions_in-cond_value  =  wa_zsdyt0049-dif_prc_neg.
*        wa_order_conditions_in-currency    =  wa_zsdyt0050-waers.
        clear: vg_value.
        if wa_zsdyt0049-DIF_PRC_NEG > 0.
          vg_value = ( wa_zsdyt0049-dif_prc_neg ) * -1.
         else.
          vg_value = wa_zsdyt0049-DIF_PRC_NEG.
         endif.
         wa_order_conditions_in-condvalue   =  vg_value.
         wa_order_conditions_in-CURRENCY_2  =  wa_zsdyt0050-waers.
        if wa_order_conditions_in-condvalue > 0 OR wa_order_conditions_in-condvalue < 0.
        APPEND wa_order_conditions_in TO it_order_conditions_in.
        endif.
        CLEAR wa_order_conditions_in.

        "Condição YV02 - DESC_RET_POS
        wa_order_conditions_in-itm_number  =  '000010'.
        wa_order_conditions_in-cond_type   =  'YV02'.

"Ajuste 14/05/2024 / BUG SOLTO 140788 / AOENNING.
*        wa_order_conditions_in-cond_value =  wa_zsdyt0049-desc_ret_pos.
*        wa_order_conditions_in-currency   =  wa_zsdyt0050-waers.
        wa_order_conditions_in-condvalue   =  wa_zsdyt0049-desc_ret_pos.
        wa_order_conditions_in-CURRENCY_2  =  wa_zsdyt0050-waers.
        if wa_order_conditions_in-condvalue > 0.
        APPEND wa_order_conditions_in TO it_order_conditions_in.
        endif.
        CLEAR wa_order_conditions_in.

        "Condição YV02 - DESC_RET_POS
        wa_order_conditions_in-itm_number  =  '000010'.
        wa_order_conditions_in-cond_type   =  'YV02'.

"Ajuste 14/05/2024 / BUG SOLTO 140788 / AOENNING.
*        wa_order_conditions_in-cond_value =  wa_zsdyt0049-desc_ret_neg.
*        wa_order_conditions_in-currency   =  wa_zsdyt0050-waers.
        clear: vg_value.
        if wa_zsdyt0049-desc_ret_neg > 0 .
          vg_value = wa_zsdyt0049-desc_ret_neg * -1.
        else.
          vg_value = wa_zsdyt0049-desc_ret_neg.
         endif.

        wa_order_conditions_in-condvalue   =  vg_value.
        wa_order_conditions_in-CURRENCY_2  =  wa_zsdyt0050-waers.
        if wa_order_conditions_in-condvalue > 0 OR wa_order_conditions_in-condvalue < 0.
        APPEND wa_order_conditions_in TO it_order_conditions_in.
        endif.
        CLEAR wa_order_conditions_in.

    ENDCASE.

    "Parceiros
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdyt0049-kunnr
      IMPORTING
        output = wa_order_partners-partn_numb.

    wa_order_partners-partn_role = 'AG'.
    APPEND wa_order_partners TO it_order_partners.

    wa_order_partners-partn_role = 'RG'.
    APPEND wa_order_partners TO it_order_partners.

    wa_order_partners-partn_role = 'WE'.
    APPEND wa_order_partners TO it_order_partners.

*    IF  wa_zsdyt0050-tp_movto = 'D'.
*      business_object = 'BUS2096'.
*    ELSE.
*      business_object = 'BUS2094'.
*    ENDIF.

    CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
      EXPORTING
        sales_header_in     = order_header_in
        business_object     = business_object
      IMPORTING
        salesdocument_ex    = salesdocument
      TABLES
        return              = it_return
        sales_items_in      = order_items_in
        sales_partners      = it_order_partners
        sales_schedules_in  = it_schedules
        sales_conditions_in = it_order_conditions_in.

    IF ( salesdocument <> space ) .

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait   = 'X'
        IMPORTING
          return = return_commit.

      IF ( return_commit[] IS INITIAL ).

        UPDATE zsdyt0049
           SET vbeln         = salesdocument
               rg_atualizado = 'S'
         WHERE obj_key EQ wa_zsdyt0049-obj_key.

        msg = 'Documentos Gerados.'.

        PERFORM: mensagem USING wa_zsdyt0049-obj_key
                                wa_zsdyt0049-interface
                                ''
                                'S'
                                msg
                                salesdocument
                                ''
                                ''
                                ''
                                'OV'.

        CLEAR : vl_erro.
      ENDIF.

    ELSE.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
        IMPORTING
          return = return_rollback.

      CLEAR: msg.

      vl_erro = 'X'.
*      concatenate 'Ordem de Venda não gerada' wa_zsdyt0049-vbeln into msg.
      LOOP AT it_return[] INTO wa_return WHERE type = 'E'.
        PERFORM: mensagem USING wa_zsdyt0049-obj_key
                                wa_zsdyt0049-interface
                                wa_zsdyt0049-vbeln
                                'E'
                                wa_return-message
                                ''
                                ''
                                ''
                                ''
                                'SD'.

      ENDLOOP.
    ENDIF.

  ELSE.

    CLEAR: msg.

    CONCATENATE 'Sem parametrização' wa_zsdyt0049-vbeln INTO msg.

    PERFORM: mensagem USING wa_zsdyt0049-obj_key
                            wa_zsdyt0049-interface
                            wa_zsdyt0049-vbeln
                            'E'
                            msg
                            ''
                            ''
                            ''
                            ''
                            'SD'.

  ENDIF.

ENDFORM.                    " DADOS_OV
*&---------------------------------------------------------------------*
*&      Form  CRIACAO_FATURAMENTO
*&---------------------------------------------------------------------*
FORM criacao_faturamento USING wa_zsdyt0049 STRUCTURE wa_zsdyt0049.

  DATA: it_messtab TYPE TABLE OF bdcmsgcoll,
        wa_messtab TYPE bdcmsgcoll,
        opt        TYPE ctu_params,
        vl_mode    TYPE c LENGTH 1,
        vl_doc_fat TYPE zsdyt0049-doc_fat.

  DATA: wa_vbrp TYPE vbrp,

        wa_lin  TYPE j_1bnflin,
        wa_doc  TYPE j_1bnfdoc.

  IF NOT ( wa_zsdyt0049 IS INITIAL ).

    IF ( wa_zsdyt0049-vbeln IS INITIAL ).
      wa_zsdyt0049-vbeln = salesdocument.
    ENDIF.

    REFRESH: it_bdc.

    PERFORM batch_input_data USING:  'X' 'SAPMV60A' '0102',
                                     ' ' 'KOMFK-VBELN(01)'  wa_zsdyt0049-vbeln,
                                     ' ' 'BDC_OKCODE'       '=SICH'.
    opt-dismode = 'E'.
    opt-defsize = 'X'.

    vl_mode = 'N'.

    CALL TRANSACTION 'VF01'
       USING it_bdc
        UPDATE 'S'
      MODE vl_mode
    MESSAGES INTO it_messtab.

    CLEAR: msg, wa_vbrp, wa_messtab.

    "READ TABLE IT_MESSTAB INTO WA_MESSTAB WITH KEY MSGTYP = 'S'.

    "IF NOT WA_MESSTAB-MSGV1 IS INITIAL.

    "  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    "    EXPORTING
    "      INPUT  = WA_MESSTAB-MSGV1
    "    IMPORTING
    "      OUTPUT = VL_DOC_FAT.

    "ELSE.
    CLEAR: wa_vbrp, vl_doc_fat.
    SELECT SINGLE * FROM vbrp INTO wa_vbrp WHERE aubel EQ wa_zsdyt0049-vbeln AND DRAFT = SPACE .
    vl_doc_fat = wa_vbrp-vbeln.
    "ENDIF.

    IF NOT vl_doc_fat IS INITIAL."Se gerou a fatura

      UPDATE zsdyt0049
         SET doc_fat       = vl_doc_fat
             rg_atualizado = 'C' "Status para definir que criou a fatura porém esta pendente a geração da fatura eletronica
             rg_pdf_fatura = 'N' "Não gerado o PDF da Fatura
       WHERE obj_key EQ wa_zsdyt0049-obj_key.

      msg = 'Documentos Gerados.'.
      PERFORM: mensagem USING wa_zsdyt0049-obj_key wa_zsdyt0049-interface '' 'S' msg wa_zsdyt0049-vbeln wa_vbrp-vbeln '' '' 'OV'.
      COMMIT WORK.
    ELSE.

      CONCATENATE 'Não foi gerado fatura para remessa' wa_zsdyt0049-vbeln INTO msg SEPARATED BY space.
      vl_erro = 'X'.

      UPDATE zsdyt0049
         SET rg_atualizado = 'N' "Vai tentar gerar a Fatura novamente.
       WHERE obj_key EQ wa_zsdyt0049-obj_key.

      LOOP AT it_messtab INTO wa_messtab WHERE msgtyp = 'E'.
        PERFORM: mensagem USING wa_zsdyt0049-obj_key wa_zsdyt0049-interface wa_zsdyt0049-vbeln 'E' msg wa_messtab-msgv1 wa_messtab-msgv2 wa_messtab-msgv3 wa_messtab-msgv4 'SD'.
      ENDLOOP.

    ENDIF.
  ENDIF.

ENDFORM.                    " CRIACAO_FATURAMENTO
*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT_DATA
*&---------------------------------------------------------------------*
FORM batch_input_data  USING    VALUE(p_flag)
                                VALUE(p_fnam)
                                VALUE(p_fval).

  CLEAR it_bdc.

  IF NOT p_flag IS INITIAL.
    it_bdc-program  = p_fnam.
    it_bdc-dynpro   = p_fval.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam = p_fnam.
    it_bdc-fval = p_fval.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    " BATCH_INPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  MENSAGEM
*&---------------------------------------------------------------------*
FORM mensagem  USING p_obj_key
                     p_interface
                     p_vbeln
                     p_type
                     p_message
                     p_msg_v1
                     p_msg_v2
                     p_msg_v3
                     p_msg_v4
                     p_id.
  DATA: wa_zob_mensagem TYPE zob_mensagem.

  CLEAR: wa_outreturn, wa_zob_mensagem.

  IF p_type = 'S'.
    READ TABLE it_outreturn[] INTO wa_outreturn WITH KEY obj_key = p_obj_key  id = p_id.
    IF sy-subrc IS INITIAL.
      DELETE it_outreturn WHERE obj_key = p_obj_key AND id = p_id.
    ENDIF.

    READ TABLE it_zob_mensagem[] INTO wa_zob_mensagem WITH KEY obj_key = p_obj_key  id = p_id.
    IF sy-subrc IS INITIAL.
      DELETE it_zob_mensagem WHERE obj_key = p_obj_key AND id = p_id.
    ENDIF.
  ENDIF.

  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = p_interface.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = p_type.
  wa_outreturn-id             = p_id.
  wa_outreturn-num            = '899'.
  wa_outreturn-message        = p_message.

  IF p_msg_v1 IS NOT INITIAL."Ordem
    wa_outreturn-message_v1 = p_msg_v1.
  ENDIF.

  IF p_msg_v2 IS NOT INITIAL."Fatura
    wa_outreturn-message_v2 = p_msg_v2.
  ENDIF.

  IF p_msg_v3 IS NOT INITIAL."Fatura Eletronica
    wa_outreturn-message_v3 = p_msg_v3.
  ENDIF.

  IF p_msg_v4 IS NOT INITIAL."Valor Fatura
    wa_outreturn-message_v4 = p_msg_v4.
  ENDIF.

  APPEND wa_outreturn TO it_outreturn.

  MOVE-CORRESPONDING wa_outreturn TO wa_zob_mensagem.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr = '01'
      object      = 'ZOB_MENSG'
    IMPORTING
      number      = wa_zob_mensagem-seq_registro.

  CALL FUNCTION 'OIL_DATE_TO_TIMESTAMP'
    EXPORTING
      i_date   = sy-datum
      i_time   = sy-uzeit
    IMPORTING
      e_tstamp = wa_zob_mensagem-timestamp.

  APPEND wa_zob_mensagem TO it_zob_mensagem.

  CLEAR wa_outreturn.

ENDFORM.                    " MENSAGEM
*&---------------------------------------------------------------------*
*&      Form   ENVIA_MENSAGEM_XI
*&---------------------------------------------------------------------*
FORM envia_mensagem_xi.

  IF it_zob_mensagem[] IS NOT INITIAL.
    MODIFY zob_mensagem FROM TABLE it_zob_mensagem.
  ENDIF.

  IF NOT it_outreturn[] IS INITIAL.

    SORT it_outreturn BY obj_key interface.

*--> 26.09.2023 22:45:28 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETURN'
*      TABLES
*        outreturn = it_outreturn[].

    DATA: lv_rfc TYPE rfcdest.

    CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_RETURN'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        AS SEPARATE UNIT
        TABLES
          outreturn = it_outreturn[].
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn[].
    ENDIF.
*<-- 26.09.2023 22:45:28 - Migração S4 – ML – Fim


  ENDIF.

  COMMIT WORK.

ENDFORM.                    "  ENVIA_MENSAGEM_XI
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_REG_LIDO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZSDYT0049  text
*----------------------------------------------------------------------*
FORM atualiza_reg_lido USING p_wa_zsdyt0049 TYPE zsdyt0049.

  p_wa_zsdyt0049-rg_atualizado = 'S'.
  MODIFY zsdyt0049 FROM p_wa_zsdyt0049.

  COMMIT WORK.

ENDFORM.                    " ATUALIZA_REG_LIDO
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_FAT_ELETRONICA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_fat_eletronica .
  DATA: wa_j_1acae  TYPE j_1acae,
        wa_bkpf     TYPE bkpf,
        vl_vrfatura TYPE bsid-dmbtr.

*  SELECT SINGLE *
*    INTO WA_BKPF
*    FROM BKPF
*   WHERE AWKEY = WA_ZSDYT0049-DOC_FAT.
*
*  SELECT SINGLE DMBTR
*    INTO VL_VRFATURA
*    FROM BSID
*   WHERE BUKRS = WA_BKPF-BUKRS
*     AND BELNR = WA_BKPF-BELNR
*     AND GJAHR = WA_BKPF-GJAHR.

  SELECT SINGLE * INTO wa_j_1acae
    FROM j_1acae
   WHERE cae_ref EQ wa_zsdyt0049-doc_fat
     AND cae_status EQ 'A'.

  IF sy-subrc IS INITIAL.
    wa_j_1acae-xblnr = wa_j_1acae-xblnr+1(15).
    UPDATE zsdyt0049
       SET nfnum         = wa_j_1acae-xblnr
           vr_fatura     = vl_vrfatura
           rg_atualizado = 'S'
     WHERE obj_key EQ wa_zsdyt0049-obj_key.

    msg = 'Fatura Eletrônica gerada'.

    PERFORM: mensagem USING wa_zsdyt0049-obj_key wa_zsdyt0049-interface '' 'S' msg '' '' wa_j_1acae-xblnr vl_vrfatura 'NF'.
    COMMIT WORK.

     "--------->>>>>Envia as mensagens de erro/sucesso para o Sigam<<<<<<------------------
    PERFORM envia_mensagem_xi.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_PDF_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZSDYT0049  text
*----------------------------------------------------------------------*
FORM gerar_pdf_fatura  USING    p_0049 TYPE zsdyt0049.

  DATA: sy_subrs TYPE sy-subrc.

  DATA: p_dir_in  TYPE string,
        p_file_in TYPE string.

  CHECK p_0049-doc_fat IS NOT INITIAL.

  PERFORM entry_pdf(zsdy0004) USING p_0049-doc_fat p_dir_in p_file_in CHANGING sy_subrs.

  IF sy_subrs IS INITIAL.

    UPDATE zsdyt0049
       SET rg_pdf_fatura = 'S'
     WHERE obj_key EQ p_0049-obj_key.

    CONCATENATE p_dir_in p_file_in INTO msg.

    PERFORM: mensagem USING p_0049-obj_key p_0049-interface '' 'S' msg p_0049-doc_fat p_dir_in p_file_in '' 'PDF_NF'.
    COMMIT WORK.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERAR_CTB_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZSDYT0049  text
*----------------------------------------------------------------------*
FORM gerar_ctb_fatura  USING    p_0049 TYPE zsdyt0049.

  DATA: sy_subrs TYPE sy-subrc,
        wa_bkpf  TYPE bkpf.

  DATA: p_dir_in  TYPE string,
        p_file_in TYPE string.

  CHECK p_0049-doc_fat IS NOT INITIAL.

  SELECT SINGLE * INTO wa_bkpf
    FROM bkpf
   WHERE awtyp EQ 'VBRK'
     AND awkey EQ p_0049-doc_fat.

  IF sy_subrs IS INITIAL.

    UPDATE zsdyt0049
       SET rg_ctb_fatura = 'S'
           ctb_bukrs     = wa_bkpf-bukrs
           ctb_belnr     = wa_bkpf-belnr
           ctb_gjahr     = wa_bkpf-gjahr
     WHERE obj_key EQ p_0049-obj_key.

    CONCATENATE 'Documento Contábil Gerado' wa_bkpf-bukrs wa_bkpf-belnr wa_bkpf-gjahr INTO msg SEPARATED BY space.
    PERFORM: mensagem USING p_0049-obj_key p_0049-interface '' 'S' msg wa_bkpf-bukrs wa_bkpf-belnr wa_bkpf-gjahr wa_bkpf-budat 'CTB_NF'.
  ELSE.
    msg = 'Documento Contábil Não Gerado'.
    PERFORM: mensagem USING p_0049-obj_key p_0049-interface '' 'E' msg '' '' '' '' 'CTB_NF'.
  ENDIF.

  COMMIT WORK.

ENDFORM.
