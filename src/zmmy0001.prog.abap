*&------------P R O J E T O  LA EXPANSION    -   M A G G I-------------*
* Programa   : ZMMY0001                                                *
* Descrição  : 1.1  Requisição de Compras - Serviços                   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                           Data: 31.07.2012 *
* Observações: Desenvolvimento inicial do Programa                     *
*&---------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :  Camila Brand                         Data: 13.08.2012  *
* Observações:    Servico                                              *
*----------------------------------------------------------------------*

report  zmmy0001.

************************************************************************
* Tabelas Internas
************************************************************************
data: begin of requisition_items occurs 10.
        include structure bapiebanc.
data: end of requisition_items.

data: begin of requisition_account_assignment occurs 10.
        include structure bapiebkn.
data: end of requisition_account_assignment.

data: begin of requisition_services occurs 10.
        include structure bapiesllc.
data: end of requisition_services.

data: begin of return occurs 10.
        include structure bapireturn.
data: end of return.


data:
      it_zmmt0023      type table of zmmt0023 with header line ,
      it_zmmt0024      type table of zmmt0024 with header line ,
      it_zmmt0024_aux  type table of zmmt0024 with header line ,
      wa_zmmt0023      type table of zmmt0023 with header line ,
      wa_zmmt0024      type table of zmmt0024 with header line,
      wa_zmmt0024_aux  type table of zmmt0024 with header line,
      wa_bapiebanc     type standard table of bapiebanc with header line,
      wa_bapiebkn      type standard table of bapiebkn  with header line,
      wa_bapiesllc     type standard table of bapiesllc with header line,
      it_outreturn     type table of zfie_ret_document,
      wa_outreturn     type zfie_ret_document,
      wa_return        type bapireturn,
      vg_index         type sy-tabix.

data ld_line_no like bapiesllc-line_no value '0'.
data ld_new_service_pckg type xflag value 'X'.
data ld_pack_no like bapiesllc-line_no value '0'.
data ld_outl_ind type xflag.
data: pckg_no like wa_bapiesllc-pckg_no.


data: v_accasserial_no like bapiebkn-serial_no value 01.
data: v_req_services_line_no like bapiesllc-line_no   value 1.
data: v_req_services_ext_line like bapiesllc-ext_line value 10.
data: v_packno like bapiebanc-pckg_no value 0000000001.
data: v_item_no like bapiebanc-preq_item value '00001'.
data: w_number             like  bapiebanc-preq_no.



*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
constants:
c_e         type c value 'I',
c_x         type c value 'X',
c_mm        type zfie_ret_document-id         value 'MM',
c_899       type zfie_ret_document-num        value '899',
c_29        type zfie_ret_document-interface  value '29'.


*----------------------------------------------------------------------*
start-of-selection.
*----------------------------------------------------------------------*
  perform: z_seleciona_dados,  " Seleção de Dados
           z_envia_log_legado. "Envia retorno de msg para o legado

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciona Dados
*----------------------------------------------------------------------*
form z_seleciona_dados .

  data: vMensagem type String,
        VG_JOB    TYPE I,
        vtype     type bapireturn-type.

  VG_JOB = 1.
*  SELECT SINGLE COUNT(*) INTO VG_JOB
*    FROM TBTCO
*   WHERE JOBNAME EQ 'ENT_ESTOQUE_GRAOS'
*     AND STATUS EQ 'R'.

*  call function 'ENQUE_READ2'
*    exporting
*      gname = 'ZIB_CONTABIL'
*    tables
*      enq   = raw_enq.

  IF ( VG_JOB EQ 1 ).

    select *
      from zmmt0024
      into table it_zmmt0024
     where rg_atualizado eq 'N'.

    select *
      from zmmt0023
      into table it_zmmt0023
       for all entries in it_zmmt0024
     where cod_oper_geo eq it_zmmt0024-cod_oper_geo.


    loop at it_zmmt0024 into wa_zmmt0024.

      wa_zmmt0024-rg_atualizado = 'S'.

      modify zmmt0024 from wa_zmmt0024.

      commit work.

      vg_index = sy-tabix.

      read table it_zmmt0023 into wa_zmmt0023 with key cod_oper_geo  = wa_zmmt0024-cod_oper_geo.

      if wa_zmmt0023-COD_OPER_GEO is initial.
         CONCATENATE 'Código de Operação ' wa_zmmt0024-cod_oper_geo ', não encontrado no SAP.' into vMensagem.

          perform z_prepara_mensagem using wa_zmmt0024-objkey
                                     'E'
                                     vMensagem
                                     ''.
          continue.
       endif.
*      call function 'NUMBER_GET_NEXT'
*        exporting
*          nr_range_nr        = '01'
*          object             = 'SERVICE'
*        importing
*          number             = pckg_no
*        exceptions
*          interval_not_found = 1
*          others             = 8.

      "REQUISITION_ITEMS
      wa_bapiebanc-preq_item  =  wa_zmmt0024-bnfpo.
      wa_bapiebanc-doc_type   =  wa_zmmt0024-bsart.
      wa_bapiebanc-pur_group  =  wa_zmmt0024-ekgrp.
      wa_bapiebanc-preq_name  =  wa_zmmt0024-afnam.
      wa_bapiebanc-short_text =  wa_zmmt0024-txz01.
      wa_bapiebanc-plant      =  wa_zmmt0024-werks.
      wa_bapiebanc-trackingno =  wa_zmmt0024-bednr.
      wa_bapiebanc-mat_grp    =  wa_zmmt0024-matkl.
      wa_bapiebanc-quantity   =  wa_zmmt0024-menge.
      wa_bapiebanc-unit       =  'UA'.
      wa_bapiebanc-acctasscat =  wa_zmmt0024-knttp.
      wa_bapiebanc-item_cat   =  wa_zmmt0024-pstyp.
      wa_bapiebanc-fixed_vend =  wa_zmmt0024-flief.
      wa_bapiebanc-purch_org  =  wa_zmmt0024-ekorg.
      wa_bapiebanc-deliv_date =  sy-datum.
      wa_bapiebanc-pckg_no    =  '0000001442'. " pckg_no + 1 . "


      "FIM REQUISITION_ITEMS
      append wa_bapiebanc to requisition_items.

      clear: wa_bapiebanc.


      "REQUISITION_ACCOUNT_ASSIGNMENT

      wa_bapiebkn-serial_no = v_accasserial_no.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zmmt0024-bnfpo
        importing
          output = wa_bapiebkn-preq_item.

      wa_bapiebkn-g_l_acct  = wa_zmmt0023-saknr.
      if wa_zmmt0024-werks(1) = 'C' .
        wa_bapiebkn-bus_area   = 'F001'.
      else.
         wa_bapiebkn-bus_area  = wa_zmmt0024-werks.
      endif.
      wa_bapiebkn-cost_ctr  = wa_zmmt0024-kostl.
      wa_bapiebkn-ORDER_NO  = wa_zmmt0024-AUFNR.

      "FIM REQUISITION_ACCOUNT_ASSIGNMENT

      append wa_bapiebkn to requisition_account_assignment.

      clear: wa_bapiebkn.


      "REQUISITION_SERVICES
      "1.3.1. First line: Header line for service specifications note 420331

      wa_bapiesllc-pckg_no    =  '0000001442'. "pckg_no. " '
      wa_bapiesllc-line_no    =  '0000000001'.
      wa_bapiesllc-ext_line   =  '0000000000'.
      wa_bapiesllc-subpckg_no =  '0000001443'. "pckg_no + 1."

      append wa_bapiesllc to requisition_services.
      clear: wa_bapiesllc.


      "1.3.2. Second line: Service with master
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = wa_zmmt0024-bnfpo
        importing
          output = wa_bapiesllc-ext_line.


      wa_bapiesllc-pckg_no     =    '0000001443'. "  pckg_no + 1.
      wa_bapiesllc-line_no     =    '0000000002'.
      wa_bapiesllc-subpckg_no  =    '0000000000'.
      wa_bapiesllc-service     =    wa_zmmt0023-srvpos.
      wa_bapiesllc-quantity    =    wa_zmmt0024-menge.
      wa_bapiesllc-gr_price    =    wa_zmmt0024-brtwr.


      call function 'CONVERSION_EXIT_CUNIT_INPUT'
        exporting
          input    = wa_zmmt0023-unit
          language = sy-langu
        importing
          output   = wa_zmmt0023-unit.

      wa_bapiesllc-base_uom    =    wa_zmmt0023-unit.

      "FIM REQUISITION_SERVICES
      append wa_bapiesllc to requisition_services.
      clear: wa_bapiesllc.

"*---> 06/07/2023 - Migração S4 - LO
*      call function 'BAPI_REQUISITION_CREATE'
      call function '/MIGNOW/FM_MM_PR_CREATE_DP'
"*<--- 06/07/2023 - Migração S4 - LO
       importing
          number                         = w_number

        tables
          requisition_items              = requisition_items
          requisition_account_assignment = requisition_account_assignment
          requisition_services           = requisition_services
          return                         = return.

      "Verifica se a BAPI foi executada sem erros e commita
      read table return into wa_return with key type = c_e.
      if sy-subrc eq 0.

        wa_zmmt0024-banfn = w_number.
        modify zmmt0024 from wa_zmmt0024 .

        call function 'BAPI_TRANSACTION_COMMIT'
          exporting
            wait = c_x.
      else.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*         IMPORTING
*           RETURN        =
                  .
      endif.

      " Envia mensagem de erro e de sucesso.
      loop at return.

        if return-type = 'I' .
          vtype = 'S'.
        else.
          vtype = return-type.
        endif.

        perform z_prepara_mensagem using wa_zmmt0024-objkey
                                         vtype
                                         return-message
                                         return-message_v1 .
      endloop.

      clear: wa_zmmt0023,
             wa_return.

      refresh: requisition_items[],
               requisition_account_assignment[],
               requisition_services[],
               return[].

    endloop.

  endif.


endform.             " Z_SELECIONA_DADOS


*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       Trata mensagens para serem enviadas para o legado
*----------------------------------------------------------------------*
form z_prepara_mensagem using pobj_key
                              ptype
                              pmessage
                              pMESSAGE_V1 .

  clear wa_outreturn.

  wa_outreturn-obj_key        = pobj_key.
  wa_outreturn-interface      = c_29.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.
  wa_outreturn-type           = ptype.
  wa_outreturn-id             = c_mm.
  wa_outreturn-num            = c_899.
  wa_outreturn-message        = pmessage.
  wa_outreturn-message_v1     = pMESSAGE_V1.

  append wa_outreturn to it_outreturn.


endform.                    "z_prepara_mensagem

*&---------------------------------------------------------------------*
*&      Form  Z_ENVIA_LOG_LEGADO
*&---------------------------------------------------------------------*
*       Envia log para o legado
*----------------------------------------------------------------------*
form z_envia_log_legado .

* Chamar função assíncrona de retorno, confirmando a gravação
* de dados
  if not it_outreturn[] is initial.
    sort it_outreturn by obj_key interface.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
*    call function 'Z_FI_OUTBOUND_RETURN' in background task
*      destination 'XI_SIGAM_RETURN'
*      tables
*        outreturn = it_outreturn.

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
          outreturn = it_outreturn.
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          outreturn = it_outreturn.
    ENDIF.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
    commit work.

  endif.

endform.                    " Z_ENVIA_LOG_LEGADO
