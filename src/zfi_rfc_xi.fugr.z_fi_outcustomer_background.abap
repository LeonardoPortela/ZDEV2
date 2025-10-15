FUNCTION z_fi_outcustomer_background.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_KNA1) LIKE  KNA1 STRUCTURE  KNA1
*"     VALUE(I_KNB1) LIKE  KNB1 STRUCTURE  KNB1
*"     VALUE(UPD_KNA1) LIKE  CDPOS-CHNGIND
*"     VALUE(UPD_KNB1) LIKE  CDPOS-CHNGIND
*"     VALUE(UPD_KNVK) LIKE  CDPOS-CHNGIND
*"  TABLES
*"      T_XKNBK STRUCTURE  FKNBK OPTIONAL
*"      T_YKNBK STRUCTURE  FKNBK OPTIONAL
*"      T_XKNVK STRUCTURE  FKNVK OPTIONAL
*"      T_YKNVK STRUCTURE  FKNVK OPTIONAL
*"----------------------------------------------------------------------

  CHECK sy-mandt = '160' OR
        sy-mandt = '060' OR
        sy-mandt = '300'.

  WAIT UP TO 2 SECONDS.

  REFRESH: it_customer, it_bank.
  CLEAR wa_customer.
  wa_customer-dt_atualizacao = sy-datum.
  wa_customer-hr_atualizacao = sy-uzeit.
  wa_customer-cd_transacao   = sy-tcode.

  DO 5 TIMES.
*> Checa se houve alterações nos dados de cliente usados pelo SIGAM
    CASE sy-index.
      WHEN  1. CONCATENATE c_fix 'KNA1'  INTO vg_campo.
      WHEN  2. CONCATENATE c_fix 'KNB1'  INTO vg_campo.
      WHEN  3. CONCATENATE c_fix 'KNVK'  INTO vg_campo.
*>    when  4. concatenate c_fix 'KNBK'  into vg_campo.
    ENDCASE.

    ASSIGN (vg_campo) TO <fs_update>.
    IF ( <fs_update> IS ASSIGNED ) AND ( NOT <fs_update> IS INITIAL ).
      wa_customer-st_atualizacao =  <fs_update>.
      EXIT.
    ENDIF.
  ENDDO.

  PERFORM f_fill_structures TABLES t_xknbk t_yknbk t_xknvk
                           CHANGING i_kna1 i_knb1.

  CHECK ( NOT i_kna1-kunnr IS INITIAL ).

*> A estrutura de endereços contida em KNA1 é do tipo SADR, por isso
*> é necessário encontrar os dados como estão armazenados na estrutura
*> ADRC, e assim copiar para os campos correspondentes.

*> Monta a chave para a busca do Id do endereço do cliente.
  CLEAR: vg_return, wa_addselect.
  REFRESH: it_addselect, it_addvalue.

  CONCATENATE sy-mandt i_kna1-kunnr INTO vg_chave.

  SELECT appl_table appl_field addrnumber FROM adrv UP TO 1 ROWS
            INTO (vg_appl_table, vg_appl_field, wa_addselect-addrnumber)
           WHERE ( appl_table EQ 'KNA1'   )
             AND ( appl_field EQ 'ADRNR'  )
             AND ( appl_key   EQ vg_chave )
             AND ( owner      EQ c_mark   )
           ORDER BY appl_table.
    APPEND wa_addselect TO it_addselect.

  ENDSELECT.



*> Com o id encontrado, carregar as informações de endereço pertinentes.

  CALL FUNCTION 'ADDR_GET_ARRAY'
    IMPORTING
      returncode        = vg_return
    TABLES
      address_selection = it_addselect
      address_value     = it_addvalue
    EXCEPTIONS
      parameter_error   = 1
      internal_error    = 2
      OTHERS            = 3.


  IF ( sy-subrc NE 0 ) OR ( NOT vg_return IS INITIAL ).
    wa_customer-id_cliente         = i_kna1-kunnr.
    wa_customer-st_atualizacao     = 'E'.
  ELSE.

    LOOP AT t_xknbk INTO wa_xknbk.
      grava_bank.
    ENDLOOP.

    LOOP AT t_yknbk INTO wa_xknbk
                   WHERE ( kz EQ ' ' ).
      wa_xknbk-kz = 'D'.
      grava_bank.
    ENDLOOP.
*> Campos correspondentes a estrutura KNA1
    wa_customer-id_cliente        = i_kna1-kunnr.

* RJF - Ini - Ajustar interface de integração do cadastro de cliente/fornecedor
    SELECT businesspartner
      UP TO 1 ROWS
    FROM ibupacustomer
    INTO @DATA(lv_ibupacustomer)
    WHERE customer EQ @i_kna1-kunnr.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE partner
      FROM but000
        INTO @DATA(lv_partner)
      WHERE partner EQ @lv_ibupacustomer
        AND type EQ '1'.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE staff_grp
        FROM bp001
        INTO @DATA(lv_staff_grp)
        WHERE partner EQ @lv_partner.
      ENDIF.
    ENDIF.
* RJF - Fim - Ajustar interface de integração do cadastro de cliente/fornecedor

    wa_customer-gr_conta          = i_kna1-ktokd.
    wa_customer-tx_tratamento     = i_kna1-anred.
    wa_customer-descricao         = i_kna1-name1.
    wa_customer-descricao2        = i_kna1-name2.
    wa_customer-tr_pesquisa       = i_kna1-sortl.
    wa_customer-cnpj              = i_kna1-stcd1.
    wa_customer-fl_pessoa_fisica  = i_kna1-stkzn.
    wa_customer-cpf               = i_kna1-stcd2.
    wa_customer-insestadual       = i_kna1-stcd3.
    wa_customer-insmunicipal      = i_kna1-stcd4.
    wa_customer-stcd5             = i_kna1-stcd5.

    IF lv_staff_grp IS INITIAL.
      wa_customer-setor_industrial  = i_kna1-brsch. "RJF
    ELSE.
      wa_customer-setor_industrial  = lv_staff_grp.  "RJF
    ENDIF.

    wa_customer-nu_celular        = i_kna1-telf2.

    wa_customer-bl_toda_empresa   = i_kna1-sperr.
    wa_customer-el_toda_area      = i_kna1-loevm.
    wa_customer-el_dados          = i_kna1-nodel.

    wa_customer-vat               = i_kna1-stceg.

    wa_customer-pais              = i_kna1-land1.
*> Campos correspondentes à estrutura KNB1
    wa_customer-empresa           = i_knb1-bukrs.
    wa_customer-cn_reconciliacao  = i_knb1-akont.
    wa_customer-ch_ordenacao      = i_knb1-zuawa.
    wa_customer-gr_administracao  = i_knb1-fdgrv.
    wa_customer-id_sigam          = i_knb1-altkn.
    wa_customer-ch_pagamento      = i_knb1-zterm.
    wa_customer-fr_pagamento      = i_knb1-zwels.
    wa_customer-suframa           = i_knb1-kverm.
*>  wa_customer-cd_imposto_retido = i_knb1-qland.

    SELECT smtp_addr FROM adr6 UP TO 1 ROWS
                     INTO wa_customer-email
                    WHERE ( addrnumber EQ wa_addselect-addrnumber ).
    ENDSELECT.

    wa_customer-bl_empresa        = i_knb1-sperr.
    wa_customer-el_empresa        = i_knb1-loevm.
    wa_customer-el_empresa_dados  = i_knb1-nodel.

*> Campos correspondentes à estrutura ADRC
    READ TABLE it_addvalue INTO wa_addvalue INDEX 1.

    wa_customer-rua               = wa_addvalue-street.
    wa_customer-numero            = wa_addvalue-house_num1.
    wa_customer-bairro            = wa_addvalue-city2.
    wa_customer-cd_postal         = wa_addvalue-post_code1.
    wa_customer-cidade            = wa_addvalue-city1.
    wa_customer-estado            = wa_addvalue-region.
    wa_customer-cx_postal         = wa_addvalue-po_box.
    wa_customer-cd_caixa_postal   = wa_addvalue-post_code2.
    wa_customer-idioma            = wa_addvalue-langu.
    wa_customer-nu_telefone       = wa_addvalue-tel_number.
    wa_customer-nu_ramal          = wa_addvalue-tel_extens.
    wa_customer-nu_fax            = wa_addvalue-fax_number.
    wa_customer-nu_ramal_fax      = wa_addvalue-fax_extens.
    wa_customer-mo_comunicacao    = wa_addvalue-deflt_comm.
    wa_customer-ob_endereco       = wa_addvalue-remark.

    wa_customer-sala_edificio     = wa_addvalue-building.
    wa_customer-sala              = wa_addvalue-roomnumber.
    wa_customer-andar             = wa_addvalue-floor.

*> Campos correspondentes à estrutura KNVK
    READ TABLE t_xknvk INTO wa_xknvk INDEX 1.
    wa_customer-nome              = wa_xknvk-namev.
    wa_customer-ds_nome           = wa_xknvk-name1.
    wa_customer-dp_contato        = wa_xknvk-abtnr.
    wa_customer-fn_contato        = wa_xknvk-pafkt.

  ENDIF.

  APPEND wa_customer TO it_customer.

*--> 23.08.2023 01:52:49 - Migração S4 – ML - Início
*  call function 'Z_FI_OUTBOUND_CUSTOMER' in background task
*    destination 'XI_SIGAM_CUSTOMER'
*    as separate unit
*    tables
*      outcustomer = it_customer
*      outbank     = it_bank.

  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_CUSTOMER'.

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
        outcustomer = it_customer
        outbank     = it_bank.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outcustomer = it_customer
        outbank     = it_bank.
  ENDIF.

  COMMIT WORK.

*<-- 23.08.2023 01:52:49 - Migração S4 – ML – Fim
ENDFUNCTION.
