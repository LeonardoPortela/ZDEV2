************************************************************************
*     P R O J E T O  C R E S C E R   -   M A G G I                     *
*                                                                      *
************************************************************************
* Consultoria ...: Braxis It Services                                  *
* Responsável ...: Geraldo Márcio Santos de Santana - Consultor ABAP   *
* Data desenv ...: 07.05.2007                                          *
* Tipo de prg ...: Interface de dados RFC
* Objetivo    ...: Disponibilizar dados de clientes em interface out-  *
*                  bound para o Sistema SIGAM                          *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 07.05.2007    Geraldo M S Santana  First Code                        *
*                                                                      *
*                                                                      *
*                                                                      *
*                                                                      *
************************************************************************
*
*
FUNCTION-POOL zfi_rfc_xi.

*----------------------------------------------------------------------*
* Declaração Geral - Constants, field-symbols, macros, etc.
*----------------------------------------------------------------------*
*
CONSTANTS: c_fix(04) TYPE c                  VALUE 'UPD_',
           c_mark    TYPE c                  VALUE 'X',
           c_dorigin LIKE reguh-dorigin      VALUE 'FI-AP',
           c_xvorl   LIKE reguh-xvorl        VALUE ' ',
           c_koart   LIKE regup-koart        VALUE 'K',
           c_blart   LIKE bkpf-blart         VALUE 'AB',
           c_busact  LIKE bapiache09-bus_act VALUE 'RFBU'.

FIELD-SYMBOLS: <fs_update> TYPE any,
               <fs_table>  TYPE table.

DATA: v_message1 TYPE balm-msgv1,
      v_message2 TYPE balm-msgv2,
      v_message3 TYPE balm-msgv3,
      vg_msgno   LIKE t100-msgnr,
      vg_koart   LIKE bseg-koart,
      wa_empresa LIKE t001,
      r_gjhar    LIKE RANGE OF bseg-gjahr WITH HEADER LINE. " Exemplo zfib005

DEFINE append_reverse_log.
  CLEAR wa_reverse_log.
  MOVE-CORRESPONDING wa_reverse TO wa_reverse_log.
  wa_reverse_log-cd_transacao   = &1.
  wa_reverse_log-type           = &2.
  wa_reverse_log-id             = &3.
  wa_reverse_log-num = vg_msgno = &4.
  wa_reverse_log-message_v1     = &5.
  wa_reverse_log-message_v2     = &6.
  wa_reverse_log-message_v3     = &7.

  v_message1 = wa_reverse_log-message_v1.
  v_message2 = wa_reverse_log-message_v2.
  v_message3 = wa_reverse_log-message_v3.

  CALL FUNCTION 'MESSAGE_PREPARE'
    EXPORTING
      language = 'P'
      msg_id   = wa_reverse_log-id
      msg_no   = vg_msgno
      msg_var1 = v_message1
      msg_var2 = v_message2
      msg_var3 = v_message3
    IMPORTING
      msg_text = wa_reverse_log-message.

  APPEND wa_reverse_log TO it_reverse_log.
END-OF-DEFINITION.

DEFINE grava_bank.
  CLEAR wa_bank.
  wa_bank-id_cliente        = wa_xknbk-kunnr.
  wa_bank-dt_atualizacao    = wa_customer-dt_atualizacao.
  wa_bank-hr_atualizacao    = wa_customer-hr_atualizacao.
  wa_bank-cd_pais_banco     = wa_xknbk-banks.
  wa_bank-ch_banco          = wa_xknbk-bankl.
  wa_bank-nu_conta_bancaria = wa_xknbk-bankn.
  wa_bank-dv_agencia        = wa_xknbk-bkont.
  wa_bank-banco_parceiro    = wa_xknbk-bvtyp.
  wa_bank-ind_ref_banco     = wa_xknbk-bkref.
  wa_bank-st_atualizacao    = wa_xknbk-kz.
  APPEND wa_bank TO it_bank.
END-OF-DEFINITION.

DEFINE grava_bank_vendor.
  CLEAR wa_bank.
  wa_bank-id_cliente        = wa_xlfbk-lifnr.
  wa_bank-dt_atualizacao    = wa_vendor-dt_atualizacao.
  wa_bank-hr_atualizacao    = wa_vendor-hr_atualizacao.
  wa_bank-cd_pais_banco     = wa_xlfbk-banks.
  wa_bank-ch_banco          = wa_xlfbk-bankl.
  wa_bank-nu_conta_bancaria = wa_xlfbk-bankn.
  wa_bank-dv_agencia        = wa_xlfbk-bkont.
  wa_bank-banco_parceiro    = wa_xlfbk-bvtyp.
  wa_bank-st_atualizacao    = wa_xlfbk-kz.
  wa_bank-ind_ref_banco     = wa_xlfbk-bkref.
  APPEND wa_bank TO it_bank.
END-OF-DEFINITION.

DEFINE grava_return.

  CLEAR wa_ret_document.
  wa_ret_document-obj_key        = wa_header-obj_key.
  "wa_ret_document-interface      = wa_document-interface.
  wa_ret_document-interface      = wa_header-interface.
  wa_ret_document-dt_atualizacao = vg_datum.
  wa_ret_document-hr_atualizacao = vg_uzeit.
  wa_ret_document-num            = wa_return-number.

  IF wa_empresa IS INITIAL OR wa_empresa-bukrs NE wa_header-bukrs.
    SELECT SINGLE * INTO wa_empresa
      FROM t001
     WHERE bukrs = wa_header-bukrs.
  ENDIF.

  IF ( wa_return IS NOT INITIAL ) AND ( wa_empresa-spras NE 'P' ).
    IF wa_return-id(1) NE 'Z'.
      CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
        EXPORTING
          id         = wa_return-id
          number     = wa_return-number
          language   = wa_empresa-spras
          textformat = 'ASC'
          message_v1 = wa_return-message_v1
          message_v2 = wa_return-message_v2
          message_v3 = wa_return-message_v3
          message_v4 = wa_return-message_v4
        IMPORTING
          message    = wa_return-message.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wa_return TO wa_ret_document.
  APPEND wa_ret_document TO it_ret_document.
END-OF-DEFINITION.

*----------------------------------------------------------------------*
* Variáveis Globais
*----------------------------------------------------------------------*
*
DATA: vg_item            LIKE bapiacar09-itemno_acc,
      vg_tcode           LIKE sy-tcode,
      vg_tcode_old       LIKE sy-tcode,
      vg_awtyp           LIKE bkpf-awtyp,
      vg_awkey           LIKE bkpf-awkey,
      vg_awsys           LIKE bkpf-awsys,
      vg_belnr           LIKE bkpf-belnr,
      vg_bukrs           LIKE bkpf-bukrs,
      vg_gjahr           LIKE bkpf-gjahr,
      vg_mandt           LIKE sy-mandt,
      vg_wrbtr           LIKE zfie_documentitem-dmbtr,
      vg_wrbtr_base      LIKE zfie_documentitem-dmbtr,
      vg_appl_table      LIKE adrv-appl_table,
      vg_appl_field      LIKE adrv-appl_field,
      vg_chave           LIKE adrv-appl_key,
      vg_return          LIKE szad_field-returncode,
      vg_campo(40)       TYPE c,
      vg_seqlan          LIKE zfie_documentheader-seqlan,
      vg_datum           LIKE sy-datum,
      vg_uzeit           LIKE sy-uzeit,
      vg_ativ            TYPE c,
      vg_mode            TYPE c,
      vg_messtab         TYPE c LENGTH 256,
      vg_jobname         LIKE tbtco-jobname,

      vg_testeabap_debug TYPE c.
*----------------------------------------------------------------------*
* Work areas e tabelas internas
*----------------------------------------------------------------------*
DATA:
  BEGIN OF it_msgtext OCCURS 0,
    texto TYPE t100-text,
  END OF it_msgtext.
*
DATA: BEGIN OF wa_tbsl,
        bschl LIKE tbsl-bschl,
        shkzg LIKE tbsl-shkzg,
        koart LIKE tbsl-koart,
      END   OF wa_tbsl,

      BEGIN OF wa_bkpf,
        mandt LIKE bkpf-mandt,
        bukrs LIKE bkpf-bukrs,
        belnr LIKE bkpf-belnr,
        gjahr LIKE bkpf-gjahr,
        awkey LIKE bkpf-awkey,
        blart LIKE bkpf-blart,
        awtyp LIKE bkpf-awtyp,
        awsys LIKE bkpf-awsys,
      END   OF wa_bkpf,

      BEGIN OF wa_reguh,
        laufd LIKE reguh-laufd,
        laufi LIKE reguh-laufi,
        xvorl LIKE reguh-xvorl,
        zbukr LIKE reguh-zbukr,
        lifnr LIKE reguh-lifnr,
        kunnr LIKE reguh-kunnr,
        empfg LIKE reguh-empfg,
        vblnr LIKE reguh-vblnr,
      END   OF wa_reguh,

      BEGIN OF wa_regup,
        vblnr LIKE regup-vblnr,
        bukrs LIKE regup-bukrs,
        belnr LIKE regup-belnr,
        gjahr LIKE regup-gjahr,
        buzei LIKE regup-buzei,
        koart LIKE regup-koart,
        lifnr LIKE regup-lifnr,
        waers LIKE regup-waers,
      END   OF wa_regup,

      BEGIN OF wa_bseg,
        augbl LIKE bseg-augbl,
        bukrs LIKE bseg-bukrs,
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
        buzei LIKE bseg-buzei,
        augdt LIKE bseg-augdt,
        nebtr LIKE bseg-nebtr,
        wrbtr LIKE bseg-wrbtr,
        wskto LIKE bseg-wskto,
        koart LIKE bseg-koart,
      END   OF wa_bseg,

      BEGIN OF wa_docaux,
        augbl LIKE bseg-augbl,
        bukrs LIKE bseg-bukrs,
        belnr LIKE bseg-belnr,
        gjahr LIKE bseg-gjahr,
        buzei LIKE bseg-buzei,
        augdt LIKE bseg-augdt,
        wrbtr LIKE bseg-wrbtr,
        wskto LIKE bseg-wskto,
        koart LIKE bseg-koart,
        nebtr LIKE bseg-nebtr,
      END   OF wa_docaux,

      BEGIN OF wa_pagamentos,
        bukrs LIKE bsak-bukrs,
        belnr LIKE bsak-belnr,
        gjahr LIKE bsak-gjahr,
        buzei LIKE bsak-buzei,
        augbl LIKE bsak-augbl,
      END   OF wa_pagamentos,

      BEGIN OF wa_nota,
        bukrs  LIKE bseg-bukrs,
        belnr  LIKE bseg-belnr,
        gjahr  LIKE bseg-gjahr,
        docnum LIKE j_1bnfdoc-docnum,
      END   OF wa_nota.

DATA: wa_accountgl       LIKE bapiacgl09,
      wa_receivable      LIKE bapiacar09,
      wa_payable         LIKE bapiacap09,
      wa_currencyamount  LIKE bapiaccr09,
      wa_accounttax      LIKE bapiactx09,
      wa_bapiret         LIKE bapiret2,
      wa_header          LIKE zfie_documentheader,
      wa_item            LIKE zfie_documentitem,
      wa_document        LIKE zfie_document,
      wa_aux             LIKE zfie_document,

      wa_returnobj       LIKE zfie_returnobj,
      wa_return          LIKE zfie_return,
      wa_ret_document    TYPE zfie_ret_document,
      wa_extension1      TYPE bapiacextc,

      wa_customer        TYPE zfie_customer,
      wa_vendor          TYPE zfie_vendor,
      wa_bank            TYPE zfie_bank,
      wa_addselect       LIKE addr1_sel,
      wa_addvalue        LIKE addr1_val,
      wa_xknvk           LIKE fknvk,
      wa_xknbk           LIKE fknbk,
      wa_xlfbk           LIKE flfbk,

      wa_message         LIKE bdcmsgcoll,
      wa_bdcdata         LIKE bdcdata,
      wa_items_payment   LIKE zfie_items_payment,
      wa_reverse         LIKE zfie_reverse_document,
      wa_reverse_ret     LIKE zfie_reverse_document_ret,
      wa_reverse_log     LIKE zfie_reverse_document_log,
      wa_tcurr           LIKE tcurr,
      wa_tcurr_aux       LIKE zmme_tcurr,
      wa_reversal        LIKE bapiacrev,
      wa_change          LIKE zfie_document_change,
      gd_documentheader  LIKE bapiache09,
      wa_dadosrfc        LIKE zfit0006,

      it_criteria        LIKE bapiackec9 OCCURS 0 WITH HEADER LINE,
      it_accountgl       LIKE STANDARD TABLE OF wa_accountgl,
      it_receivable      LIKE STANDARD TABLE OF wa_receivable,
      it_payable         LIKE STANDARD TABLE OF wa_payable,
      it_currencyamount  LIKE STANDARD TABLE OF wa_currencyamount,
      it_accounttax      LIKE STANDARD TABLE OF wa_accounttax,
      it_bapiret         LIKE STANDARD TABLE OF wa_bapiret,
      it_tbsl            LIKE STANDARD TABLE OF wa_tbsl,
      it_bkpf            LIKE STANDARD TABLE OF wa_bkpf,
      it_pagamentos      LIKE STANDARD TABLE OF wa_pagamentos,
      it_bank            LIKE STANDARD TABLE OF wa_bank,
      it_customer        LIKE STANDARD TABLE OF wa_customer,
      it_vendor          LIKE STANDARD TABLE OF wa_vendor,
      it_addselect       LIKE STANDARD TABLE OF wa_addselect,
      it_addvalue        LIKE STANDARD TABLE OF wa_addvalue,
      it_header          LIKE STANDARD TABLE OF wa_header,
      it_item            LIKE STANDARD TABLE OF wa_item,
      it_return          LIKE STANDARD TABLE OF wa_return,
      it_returnobj       LIKE STANDARD TABLE OF wa_returnobj,
      it_ret_document    LIKE STANDARD TABLE OF wa_ret_document,
      it_reguh           LIKE STANDARD TABLE OF wa_reguh,
      it_regup           LIKE STANDARD TABLE OF wa_regup,
      it_bseg            LIKE STANDARD TABLE OF wa_bseg,
      t_knvk             LIKE STANDARD TABLE OF wa_xknvk,
      it_items_payment   LIKE STANDARD TABLE OF wa_items_payment,
      it_items_payment_k LIKE STANDARD TABLE OF wa_items_payment,
      it_items_payment_d LIKE STANDARD TABLE OF wa_items_payment,
      it_tcurr_aux       LIKE STANDARD TABLE OF wa_tcurr_aux,
      it_reverse_log     LIKE STANDARD TABLE OF wa_reverse_log,
      it_reverse_ret     LIKE STANDARD TABLE OF wa_reverse_ret,
      it_change          LIKE STANDARD TABLE OF wa_change,
      it_dadosrfc        LIKE STANDARD TABLE OF wa_dadosrfc,
      it_docaux          LIKE STANDARD TABLE OF wa_docaux,
      it_extension1      LIKE STANDARD TABLE OF bapiacextc,

      it_bdcdata         LIKE STANDARD TABLE OF wa_bdcdata,
      it_message         LIKE STANDARD TABLE OF wa_message,

      it_dta             TYPE STANDARD TABLE OF bdcdata,
      wa_dta             LIKE LINE OF           it_dta.

DATA: it_msg TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wa_msg TYPE bdcmsgcoll.



DATA: wa_testetxt       TYPE string.
*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0405   text
*      -->P_0406   text
*      -->P_0407   text
*      -->P_0408   text
*      -->P_0409   text
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
  CLEAR wa_dta.
  wa_dta-program   = p_program.
  wa_dta-dynpro    = p_dynpro.
  wa_dta-dynbegin  = p_start.
  wa_dta-fnam      = p_fnam.
  wa_dta-fval      = p_fval.
  APPEND wa_dta TO it_dta.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0531   text
*      -->P_P_MODE  text
*      -->P_P_UPD  text
*----------------------------------------------------------------------*
FORM f_call_transaction USING p_trans
                              p_mode
                              p_upd.

*  DATA: BEGIN OF tl_msg OCCURS 0,
*         msg TYPE t100-text,
*         fld TYPE bdcmsgcoll-fldname,
*        END OF tl_msg.

  REFRESH: it_msg, it_msgtext.

  CALL TRANSACTION p_trans USING it_dta
    MODE p_mode
    MESSAGES INTO it_msg
    UPDATE p_upd.

  IF it_msg[] IS NOT INITIAL.
    SELECT text
      FROM t100
      INTO TABLE it_msgtext
      FOR ALL ENTRIES IN it_msg
      WHERE arbgb = it_msg-msgid AND
            msgnr = it_msg-msgnr AND
            sprsl = sy-langu.

    LOOP AT it_msgtext.
      TRANSLATE it_msgtext-texto USING '& '.
      CONDENSE it_msgtext-texto.
      MODIFY it_msgtext.
    ENDLOOP.
  ENDIF.

ENDFORM.
