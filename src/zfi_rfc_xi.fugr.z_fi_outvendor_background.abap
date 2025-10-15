FUNCTION z_fi_outvendor_background.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LFA1) LIKE  LFA1 STRUCTURE  LFA1
*"     VALUE(I_LFB1) LIKE  LFB1 STRUCTURE  LFB1
*"     VALUE(I_LFM1) LIKE  LFM1 STRUCTURE  LFM1
*"     VALUE(UPD_LFA1) LIKE  CDPOS-CHNGIND
*"     VALUE(UPD_LFB1) LIKE  CDPOS-CHNGIND
*"     VALUE(UPD_LFBK) LIKE  CDPOS-CHNGIND
*"  TABLES
*"      T_XLFBK STRUCTURE  FLFBK OPTIONAL
*"      T_YLFBK STRUCTURE  FLFBK OPTIONAL
*"----------------------------------------------------------------------

  CHECK sy-mandt = '060' OR
        sy-mandt = '160' OR
        sy-mandt = '300'.


  WAIT UP TO 2 SECONDS.

  REFRESH: it_vendor, it_bank.
  CLEAR wa_vendor.
  wa_vendor-dt_atualizacao = sy-datum.
  wa_vendor-hr_atualizacao = sy-uzeit.
  wa_vendor-cd_transacao   = sy-tcode.

  DO 5 TIMES.
*> Checa se houve alterações nos dados de cliente usados pelo SIGAM
    CASE sy-index.
      WHEN  1. CONCATENATE c_fix 'LFA1'  INTO vg_campo.
      WHEN  2. CONCATENATE c_fix 'LFB1'  INTO vg_campo.
      WHEN  3. CONCATENATE c_fix 'LFBK'  INTO vg_campo.
    ENDCASE.

    ASSIGN (vg_campo) TO <fs_update>.
    IF ( <fs_update> IS ASSIGNED ) AND ( NOT <fs_update> IS INITIAL ).
      wa_vendor-st_atualizacao =  <fs_update>.
      EXIT.
    ENDIF.
  ENDDO.

  PERFORM f_fill_structures_2 TABLES t_xlfbk t_ylfbk t_knvk
                            CHANGING i_lfa1 i_lfb1.

  CHECK ( NOT i_lfa1-lifnr IS INITIAL ).

*> A estrutura de endereços contida em LFA1 é do tipo SADR, por isso
*> é necessário encontrar os dados como estão armazenados na estrutura
*> ADRC, e assim copiar para os campos correspondentes.

*> Monta a chave para a busca do Id do endereço do cliente.
  CLEAR: vg_return, wa_addselect.
  REFRESH: it_addselect, it_addvalue.

  CONCATENATE sy-mandt i_lfa1-lifnr INTO vg_chave.

  SELECT appl_table appl_field addrnumber FROM adrv UP TO 1 ROWS
            INTO (vg_appl_table, vg_appl_field, wa_addselect-addrnumber)
           WHERE ( appl_table EQ 'LFA1'   )
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
    wa_vendor-id_fornecedor      = i_lfa1-lifnr.
    wa_vendor-st_atualizacao     = 'E'.
  ELSE.

    LOOP AT t_xlfbk INTO wa_xlfbk.
      grava_bank_vendor.
    ENDLOOP.

    LOOP AT t_ylfbk INTO wa_xlfbk
                   WHERE ( kz EQ ' ' ).
      wa_xlfbk-kz = 'D'.
      grava_bank_vendor.
    ENDLOOP.
*> Campos correspondentes a estrutura LFA1
    wa_vendor-id_fornecedor     = i_lfa1-lifnr.

* RJF - Ini - Ajustar interface de integração do cadastro de cliente/fornecedor
    SELECT businesspartner
      UP TO 1 ROWS
    FROM ibupasuplrcotp
    INTO @DATA(lv_ibupasuplrcotp)
    WHERE supplier EQ @i_lfa1-lifnr.
    ENDSELECT.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE partner
      FROM but000
        INTO @DATA(lv_partner)
      WHERE partner EQ @lv_ibupasuplrcotp
        AND type EQ '1'.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE staff_grp
        FROM bp001
        INTO @DATA(lv_staff_grp)
        WHERE partner EQ @lv_partner.
      ENDIF.
    ENDIF.
* RJF - Fim - Ajustar interface de integração do cadastro de cliente/fornecedor

    wa_vendor-gr_conta          = i_lfa1-ktokk.
    wa_vendor-tx_tratamento     = i_lfa1-anred.
    wa_vendor-descricao         = i_lfa1-name1.
    wa_vendor-tr_pesquisa       = i_lfa1-sortl.
    wa_vendor-cnpj              = i_lfa1-stcd1.
    wa_vendor-fl_pessoa_fisica  = i_lfa1-stkzn.
    wa_vendor-cpf               = i_lfa1-stcd2.
    wa_vendor-insestadual       = i_lfa1-stcd3.
    wa_vendor-insmunicipal      = i_lfa1-stcd4.
    wa_vendor-stcd5             = i_lfa1-stcd5.
    wa_vendor-id_pis            = i_lfa1-stenr.
    wa_vendor-nu_celular        = i_lfa1-telf2.
    wa_vendor-bl_toda_empresa   = i_lfa1-sperr.
    "wa_vendor-bl_empresa        = i_lfa1-sperm.
    wa_vendor-el_toda_area      = i_lfa1-loevm.
    wa_vendor-el_dados          = i_lfa1-nodel.
    wa_vendor-bl_qualidade      = i_lfa1-sperq.
    wa_vendor-rntrc             = i_lfa1-bahns.
    IF lv_staff_grp IS INITIAL.
      wa_vendor-setor_industrial  = i_lfa1-brsch. "RJF
    ELSE.
      wa_vendor-setor_industrial  = lv_staff_grp.  "RJF
    ENDIF.

    wa_vendor-vat               = i_lfa1-stceg.
    wa_vendor-pais              = i_lfa1-land1.
    wa_vendor-descricao2        = i_lfa1-name2.
    wa_vendor-fnc_bloqueio      = i_lfa1-sperq.

    " Ajuste referente as US 93939 – Integração SIGAM no projeto Asgardianos do Azure Devops / Anderson Oenning
    IF i_lfa1-j_1kftind IS NOT INITIAL.
      wa_vendor-tipo_industria = i_lfa1-j_1kftind.
    ENDIF.
    "Ajuste referente as US 93939 – Integração SIGAM no projeto Asgardianos do Azure Devops / Anderson Oenning

    wa_vendor-gbort             = i_lfa1-gbort .
    wa_vendor-gbdat             = i_lfa1-gbdat .
    wa_vendor-sexkz             = i_lfa1-sexkz .
    wa_vendor-emissor_nfe       = COND string( WHEN i_lfa1-scacd NE '8888' THEN 'X' ELSE ' ' ).

*> Campos correspondentes à estrutura KNB1
    wa_vendor-empresa           = i_lfb1-bukrs.
    wa_vendor-cn_reconciliacao  = i_lfb1-akont.
    wa_vendor-ch_ordenacao      = i_lfb1-zuawa.
    wa_vendor-gr_administracao  = i_lfb1-fdgrv.
    wa_vendor-id_sigam          = i_lfb1-altkn.
    wa_vendor-ch_pagamento      = i_lfb1-zterm.
    wa_vendor-fr_pagamento      = i_lfb1-zwels.

*>  wa_vendor-cd_imposto_retido = i_lfb1-qland.

    SELECT smtp_addr FROM adr6 UP TO 1 ROWS
                     INTO wa_vendor-email
                    WHERE ( addrnumber EQ wa_addselect-addrnumber ).
    ENDSELECT.

    wa_vendor-bl_empresa        = i_lfb1-sperr.
    wa_vendor-el_empresa        = i_lfb1-loevm.
    wa_vendor-el_empresa_dados  = i_lfb1-nodel.

*> Campos correspondentes à estrutura ADRC
    READ TABLE it_addvalue INTO wa_addvalue INDEX 1.

    wa_vendor-rua               = wa_addvalue-street.
    wa_vendor-numero            = wa_addvalue-house_num1.
    wa_vendor-bairro            = wa_addvalue-city2.
    wa_vendor-cd_postal         = wa_addvalue-post_code1.
    wa_vendor-cidade            = wa_addvalue-city1.
    wa_vendor-estado            = wa_addvalue-region.
    wa_vendor-cx_postal         = wa_addvalue-po_box.
    wa_vendor-cd_caixa_postal   = wa_addvalue-post_code2.
    wa_vendor-idioma            = wa_addvalue-langu.
    wa_vendor-nu_telefone       = wa_addvalue-tel_number.
    wa_vendor-nu_ramal          = wa_addvalue-tel_extens.
    wa_vendor-nu_fax            = wa_addvalue-fax_number.
    wa_vendor-nu_ramal_fax      = wa_addvalue-fax_extens.
    wa_vendor-mo_comunicacao    = wa_addvalue-deflt_comm.
    wa_vendor-ob_endereco       = wa_addvalue-remark.

****Inicio CS2022000746 - Ajuste RFC Z_FI_OUTBOUND_VENDOR / Anderson Oenning
    IF wa_addvalue-home_city IS NOT INITIAL.
      wa_vendor-home_city        = wa_addvalue-home_city.
    ENDIF.
****Fim CS2022000746 - Ajuste RFC Z_FI_OUTBOUND_VENDOR / Anderson Oenning

    wa_vendor-sala_edificio     = wa_addvalue-building.
    wa_vendor-sala              = wa_addvalue-roomnumber.
    wa_vendor-andar             = wa_addvalue-floor.



*> Campos correspondentes à estrutura KNVK
    READ TABLE t_knvk INTO wa_xknvk INDEX 1.
    wa_vendor-nome              = wa_xknvk-namev.
    wa_vendor-ds_nome           = wa_xknvk-name1.
    wa_vendor-dp_contato        = wa_xknvk-abtnr.
    wa_vendor-fn_contato        = wa_xknvk-pafkt.

  ENDIF.

  APPEND wa_vendor TO it_vendor.

*--> 24.08.2023 17:53:05 - Migração S4 – ML - Início
*  CALL FUNCTION 'Z_FI_OUTBOUND_VENDOR' IN BACKGROUND TASK
*    DESTINATION 'XI_SIGAM_VENDOR'
*    AS SEPARATE UNIT
*    TABLES
*      outvendor = it_vendor
*      outbank   = it_bank.
*
*  COMMIT WORK.
  DATA: lv_rfc TYPE rfcdest.

  CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_VENDOR'.

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
        outvendor = it_vendor
        outbank   = it_bank.
  ELSE.
    CALL FUNCTION c_fm IN BACKGROUND TASK
      TABLES
        outvendor = it_vendor
        outbank   = it_bank.
  ENDIF.

  COMMIT WORK.
*<-- 24.08.2023 17:53:05 - Migração S4 – ML – Fim
ENDFUNCTION.
