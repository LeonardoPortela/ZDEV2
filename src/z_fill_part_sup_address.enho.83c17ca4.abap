"Name: \TY:/MIGNOW/CL_MIGBP\ME:MT_BP_PROCESS_DATA\SE:BEGIN\EI
ENHANCEMENT 0 Z_FILL_PART_SUP_ADDRESS.
*-US 135191-18-06-2024-#135191-RJF-inicio
DATA(lv_ac_termo_al5bank) = abap_false.
IMPORT lv_ac_termo_al5bank TO lv_ac_termo_al5bank FROM MEMORY ID 'AC_TERMO_AL5BANK'.

IF lv_ac_termo_al5bank IS NOT INITIAL.
  DATA:
    lv_kunnr1 TYPE kna1-kunnr,
    lv_valid  TYPE c.

  lv_valid = abap_true.

  IF gs_lfa1 IS NOT INITIAL.

    IF gs_lfa1-lifnr IS NOT INITIAL. "UPDATE SUPPLIER

      CALL FUNCTION 'BUPA_NUMBERS_GET'
        EXPORTING
          iv_partner = gs_lfa1-lifnr  " Business Partner Number
        IMPORTING
          es_but000  = gs_but000.     " Business Partner Data

      IF gs_but000 IS NOT INITIAL.
        gv_bp_type = gs_but000-type.
      ENDIF.

      me->mt_bp_update_supplier(
        EXPORTING
          im_test       = gv_test
          im_bp_type    = gv_bp_type      " Categoria do parceiro de negócios
          is_lfa1       = gs_lfa1         " Mestre de fornecedores (parte geral)
          it_lfbk       = gt_lfbk         " Mestre de fornecedores (coordenadas do banco)
          it_lfb1       = gt_lfb1         " Mestre de fornecedores (empresa)
          it_lfbw       = gt_lfbw         " Mestre de fornecedores (empresa)
          is_rf02k      = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
          is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
          is_sza1_d0100 = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
          it_lfm1       = gt_lfm1
        IMPORTING
          et_return     = gt_return       " Tabela de retorno
          em_partner    = gv_partner      " Nº parceiro de negócios
      ).

    ELSE. " CREATE SUPPLIER
      "Cria BP
      "------------------------------------------------------------------
      me->mt_bp_create_supplier(
        EXPORTING
          im_test       = gv_test
          is_lfa1       = gs_lfa1         " Mestre de fornecedores (parte geral)
          it_lfbk       = gt_lfbk         " Interface externa: dados detalhes bancários
          it_lfb1       = gt_lfb1         " Mestre de fornecedores (empresa)
          it_lfbw       = gt_lfbw         " Mestre de fornecedores (empresa)
          is_rf02k      = gs_rf02k        " Atual.dados mestre fornecedor: campos de tela e operativos
          is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
          is_sza1_d0100 = gs_sza1_d0100   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
          it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
        CHANGING
          et_return     = gt_return       " Tabela de retorno
          em_partner    = gv_partner      " Nº parceiro de negócios
      ).
    ENDIF.

  ENDIF.

  me->mt_shdb_convert_return_bdcmsg(
    EXPORTING
      im_tcode  = gv_tcode                  " Campo do sistema ABAP: código de transação atual
      it_return = gt_return                  " Tabela de retorno
    CHANGING
      ct_bdcmsg = ct_bdcmsg                  " Tabela de mensagens na transação
  ).

  IF gv_partner IS NOT INITIAL.
    LOOP AT ct_bdcmsg ASSIGNING FIELD-SYMBOL(<fs_bdcmsg1>)
         WHERE msgtyp = 'S'.
      <fs_bdcmsg1>-msgv2 = gv_partner.
    ENDLOOP.
  ELSE.

    IF gs_kna1 IS NOT INITIAL. " Update Customer

      IF gs_kna1-name1 IS INITIAL.
        gs_kna1-name1 = gs_addr1_data-name1.
      ENDIF.
      IF gs_kna1-name3 IS INITIAL.
        gs_kna1-name3 = gs_addr1_data-name1.
      ENDIF.

      IF gs_rf02d-ktokd IS NOT INITIAL.
        gs_kna1-ktokd = gs_rf02d-ktokd.
      ENDIF.

      IF gt_knb1 IS INITIAL AND gs_knb1 IS NOT INITIAL.
        APPEND gs_knb1 TO gt_knb1.
      ENDIF.
      LOOP AT gt_knb1 ASSIGNING FIELD-SYMBOL(<fs_knb11>).
        <fs_knb11>-bukrs = gs_rf02d-bukrs.
      ENDLOOP.

      IF gt_knbw IS INITIAL AND gs_knbw IS NOT INITIAL.
        APPEND gs_knbw TO gt_knbw.
      ENDIF.
      LOOP AT gt_knbw ASSIGNING FIELD-SYMBOL(<fs_knbw1>).
        <fs_knbw1>-bukrs = gs_rf02d-bukrs.
      ENDLOOP.

      IF gt_knvi IS INITIAL AND gs_knvi IS NOT INITIAL.
        APPEND gs_knvi TO gt_knvi.
      ENDIF.
      LOOP AT gt_knvi ASSIGNING FIELD-SYMBOL(<fs_knvi1>).
        <fs_knvi1>-aland = COND #( WHEN <fs_knvi1>-aland IS INITIAL THEN 'BR'   ELSE <fs_knvi1>-aland ).
        <fs_knvi1>-tatyp = COND #( WHEN <fs_knvi1>-tatyp IS INITIAL THEN 'IBRX' ELSE <fs_knvi1>-tatyp ).
      ENDLOOP.

      IF gt_knvv IS INITIAL AND gs_knvv IS NOT INITIAL.
        APPEND gs_knvv TO gt_knvv.
      ENDIF.
      LOOP AT gt_knvv ASSIGNING FIELD-SYMBOL(<fs_knvv1>).
        <fs_knvv1>-vkorg = COND #( WHEN <fs_knvv1>-vkorg IS INITIAL THEN gs_rf02d-vkorg ELSE <fs_knvv1>-vkorg ).
        <fs_knvv1>-vtweg = COND #( WHEN <fs_knvv1>-vtweg IS INITIAL THEN gs_rf02d-vtweg ELSE <fs_knvv1>-vtweg ).
        <fs_knvv1>-spart = COND #( WHEN <fs_knvv1>-spart IS INITIAL THEN gs_rf02d-spart ELSE <fs_knvv1>-spart ).
      ENDLOOP.

      IF gt_adsmtp IS INITIAL AND gs_adsmtp IS NOT INITIAL.
        APPEND gs_adsmtp TO gt_adsmtp.
      ENDIF.

      IF ( gs_kna1-stcd1 IS NOT INITIAL OR gs_kna1-stcd2 IS NOT INITIAL ) AND
         gs_kna1-kunnr IS INITIAL.

        SELECT SINGLE kunnr FROM kna1
          INTO @lv_kunnr1
          WHERE stcd1 = @gs_kna1-stcd1
            AND stcd2 = @gs_kna1-stcd2.
      ELSE.
        lv_kunnr1 = gs_kna1-kunnr.
      ENDIF.

      IF lv_kunnr1 IS NOT INITIAL.
        CALL FUNCTION 'BUPA_NUMBERS_GET'
          EXPORTING
            iv_partner = lv_kunnr1     " Business Partner Number
          IMPORTING
            es_but000  = gs_but000.   " Business Partner Data

        gv_bp_type = gs_but000-type.

        me->mt_bp_update_customer(
          EXPORTING
            im_test       = gv_test
            im_bp_type    = gv_bp_type
            is_kna1       = gs_kna1         " Mestre de fornecedores (parte geral)
            is_knvk       = gs_knvk         " Interface externa: dados detalhes bancários
            it_knb1       = gt_knb1         " Mestre de fornecedores (empresa)
            it_knbw       = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
            it_knvi       = gt_knvi         " Mestre de clientes - indicadores de impostos
            it_knvv       = gt_knvv         " Mestre de clientes (área de vendas)
            it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
            is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
            is_addr2_data = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
            is_sza1_d0100 = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
          CHANGING
            et_return     = gt_return       " Tabela de retorno
            em_partner    = gv_partner      " Nº parceiro de negócios
        ).

      ELSE. " Create Customer

        "Cria BP
        me->mt_bp_create_customer(
          EXPORTING
            im_test       = gv_test
            is_kna1       = gs_kna1         " Mestre de fornecedores (parte geral)
            is_knvk       = gs_knvk         " Interface externa: dados detalhes bancários
            it_knb1       = gt_knb1         " Mestre de fornecedores (empresa)
            it_knbw       = gt_knbw         " Atual.dados mestre fornecedor: campos de tela e operativos
            it_knvi       = gt_knvi         " Mestre de clientes - indicadores de impostos
            it_knvv       = gt_knvv         " Mestre de clientes (área de vendas)
            it_adsmtp     = gt_adsmtp       " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
            is_addr1_data = gs_addr1_data   " Estrutura de transferência para endereços
            is_addr2_data = gs_addr2_data   " Campos tela SAPLSZA10100 (campos não existent.em ADDR1_DATA)
            is_sza1_d0100 = gs_sza1_d0100   " Estrut.transferênc.p/endereços SMTP (admin.centr.endereços)
          CHANGING
            et_return     = gt_return       " Tabela de retorno
            em_partner    = gv_partner      " Nº parceiro de negócios
        ).

      ENDIF.

      me->mt_shdb_convert_return_bdcmsg(
        EXPORTING
          im_tcode  = gv_tcode                  " Campo do sistema ABAP: código de transação atual
          it_return = gt_return                  " Tabela de retorno
        CHANGING
          ct_bdcmsg = ct_bdcmsg                  " Tabela de mensagens na transação
      ).

      IF gv_partner IS NOT INITIAL.
        LOOP AT ct_bdcmsg ASSIGNING FIELD-SYMBOL(<fs_bdcmsg_cust1>)
             WHERE msgtyp = 'S'.
          <fs_bdcmsg_cust1>-msgv2 = gv_partner.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.
  IF lv_valid IS NOT INITIAL.
    EXIT.
  ENDIF.
ENDIF.
*-US 135191-18-06-2024-#135191-RJF-fim
ENDENHANCEMENT.
