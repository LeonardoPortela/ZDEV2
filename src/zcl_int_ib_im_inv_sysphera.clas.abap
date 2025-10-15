CLASS zcl_int_ib_im_inv_sysphera DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_integracao_inject .
    INTERFACES zif_integracao_inbound .

    TYPES:
      BEGIN OF ty_branch,
        cnpj    TYPE bapibranch-cgc_number,
        name    TYPE bapibranch-name,
        country TYPE adrc-country,
        region  TYPE adrc-region,
        street  TYPE adrc-street,
        city2   TYPE adrc-city2,
        city1   TYPE adrc-city1.
    TYPES  END OF ty_branch .
    TYPES:
      BEGIN OF ty_zsdt0170,
        include TYPE zsdt0170.
    TYPES  END OF ty_zsdt0170 .
    TYPES:
      tyt_zsdt0170 TYPE TABLE OF ty_zsdt0170 .
    TYPES:
      BEGIN OF ty_conf,
*        chave_nfe   TYPE zde_chave_doc_e,
*        produto     TYPE maktx,
*        cod_clifor  TYPE lifnr,
*        nome_clifor TYPE name1_gp,
*        cpf_prod    TYPE stcd1,
*        nfenum      TYPE j_1bnfnum9,
*        series      TYPE j_1bseries,
*        docdat      TYPE j_1bdocdat,
*        netwrt      TYPE j_1bnfnett,
*        prod_cfop   TYPE zib_nfe_dist_itm-prod_cfop, "ZDE_CFOP
*        icms_cst    TYPE zib_nfe_dist_itm-icms_cst, "ZDE_CST_ICMS
*        pstdat      TYPE j_1bpstdat,
        include TYPE zfit0012.
    TYPES: END OF ty_conf .

    TYPES:
      tyt_conf TYPE TABLE OF zfit0012 .

    TYPES: BEGIN OF ty_ap_inv,
             solicitacao_invest TYPE num6,
             leanz              TYPE leanz,
           END OF ty_ap_inv.

    DATA: it_found TYPE TABLE OF ty_ap_inv.

    TYPES: BEGIN OF ty_anla,
             anln1 TYPE anln1,
             anln2 TYPE anln2,
             leanz TYPE leanz,
           END OF ty_anla.

    DATA: it_anla TYPE TABLE OF ty_anla.

    TYPES: BEGIN OF ty_out,
             posnr    TYPE posnr,
             mensagem TYPE string,
           END OF ty_out.

    DATA: git_outbound TYPE TABLE OF ty_out.

    DATA git_data_inbound TYPE zfie0016 .
    DATA gv_area TYPE kokrs .
    DATA gv_perio TYPE monat .
    DATA gv_ano TYPE gjahr .
    DATA gv_protocolo TYPE string .
    DATA gv_prot TYPE string .
    DATA git_ordens TYPE zttsys_aufnrl .
    DATA git_sysphera TYPE STANDARD TABLE OF zsys_posnr. "ZTTSYS_posnr .
    DATA it_conf TYPE tyt_conf .
    DATA:
      it_saida TYPE STANDARD TABLE OF zpme0087 .
    DATA at_id_interface TYPE zde_id_interface .
    DATA at_id_due TYPE num10 .
    DATA gv_string_job TYPE string .
    DATA gv_erro_string TYPE string .

    METHODS constructor
      RAISING
        zcx_integracao .
    METHODS get_filial_matriz
      IMPORTING
        !iv_bukrs  TYPE bukrs
      EXPORTING
        !es_branch TYPE ty_branch .
    METHODS create_job
      IMPORTING
        !iv_job TYPE btcjob
      EXPORTING
        !ev_ret TYPE char1 .
    METHODS run
      EXPORTING
        !lt_message TYPE string
        !it_conf    TYPE tyt_conf .
    METHODS set
      IMPORTING
        VALUE(i_area)    TYPE kokrs
        VALUE(i_perio)   TYPE monat
        VALUE(i_ano)     TYPE gjahr
        !i_prot          TYPE string OPTIONAL
        VALUE(it_ordens) TYPE zttsys_aufnrl .
    METHODS get_data .
    METHODS recovered_job .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_IM_INV_SYSPHERA IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD CREATE_JOB.

*    TYPES:
*      BEGIN OF ty_kokrs,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE kokrs,
*        high   TYPE kokrs,
*      END OF ty_kokrs .
*
*    TYPES:
*      BEGIN OF ty_monat,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE monat,
*        high   TYPE monat,
*      END OF ty_monat .
*
*    TYPES:
*      BEGIN OF ty_gjahr,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE gjahr,
*        high   TYPE gjahr,
*      END OF ty_gjahr .
*
*    TYPES:
*      BEGIN OF ty_aufnr,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE aufnr,
*        high   TYPE aufnr,
*      END OF ty_aufnr .
*
*    DATA:
*      ls_kokrs TYPE ty_kokrs,
*      ls_monat TYPE ty_monat,
*      ls_gjahr TYPE ty_gjahr,
*      ls_aufnr TYPE ty_aufnr,
*      lt_kokrs TYPE TABLE OF ty_kokrs,
*      lt_monat TYPE TABLE OF ty_monat,
*      lt_gjahr TYPE TABLE OF ty_gjahr,
*      lt_aufnr TYPE TABLE OF ty_aufnr.
*
*    DATA:
*      lv_jobcount         TYPE tbtcjob-jobcount,               " Identificador do job
*      lv_startdate        TYPE d,
*      lv_starttime        TYPE t,
*      lv_enddate          TYPE d,
*      lv_endtime          TYPE t,
*      ls_tvarvc           TYPE tvarvc,
*      ls_spool_parameters TYPE pri_params.
*
*    DATA: lv_ano            TYPE numc4,
*          lv_mes            TYPE numc2,
*          lv_ultimo_dia_mes TYPE endda,
*          lv_qtd_dias       TYPE tbtcjob-prddays.
*
*    DATA: lv_repname TYPE  rsvar-report.           " for variant handling
*    DATA: iv_varname TYPE  raldb-variant VALUE 'SAP_UPG_V1'.
*    DATA: iv_varianttext  TYPE  varit-vtext VALUE 'Upgrade variant'.
*    DATA: wl_subrc TYPE sy-subrc.
*    DATA: tt_reportparam TYPE TABLE OF  rsparams,
*          wa_reportparam TYPE rsparams.
*
*    DATA : c_no(1) TYPE c . "value 'N', " Criação do job
*    DATA: i_steplist  TYPE STANDARD TABLE OF tbtcstep,
*          wa_steplist TYPE tbtcstep.
*    DATA: wl_tbtcjob  TYPE  tbtcjob,
*          wl_tbtcstrt TYPE  tbtcstrt,
*          i_head      TYPE tbtcjob.
*
*    DATA: starttimeimmediate TYPE btch0000-char1 VALUE 'X',
*          lv_sdlstrtdt       TYPE btcsdate,
*          lv_sdlstrttm       TYPE btcstime.
*
*
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_area  high = '' ) TO lt_kokrs[].
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_perio high = '' ) TO lt_monat[].
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_ano   high = '' ) TO lt_gjahr[].
*
*    ls_kokrs-sign   = 'I'.
*    ls_kokrs-option = 'EQ'.
*    ls_kokrs-low    = gv_area.
*    ls_monat = ls_kokrs.
*    ls_gjahr = ls_kokrs.
*    ls_aufnr = ls_kokrs.
*    ls_monat-low = gv_perio.
*    ls_gjahr-low = gv_ano.
*
*    LOOP AT git_ordens ASSIGNING FIELD-SYMBOL(<fs_ord>).
*      IF sy-tabix EQ 1.
*        ls_aufnr-low = <fs_ord>.
*      ENDIF.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ord> high = '' ) TO lt_aufnr[].
*    ENDLOOP.
*
*    wa_reportparam-selname = 'S_KOKRS'.
*    wa_reportparam-kind    = 'S'.
*    wa_reportparam-sign    = 'I'.
*    wa_reportparam-option  = 'EQ'.
*    wa_reportparam-low = ls_kokrs-low.
*    APPEND wa_reportparam TO tt_reportparam.
*    CLEAR wa_reportparam.
*
*    wa_reportparam-selname = 'S_GJAHR'.
*    wa_reportparam-kind    = 'S'.
*    wa_reportparam-sign    = 'I'.
*    wa_reportparam-option  = 'EQ'.
*    wa_reportparam-low = ls_gjahr-low.
*    APPEND wa_reportparam TO tt_reportparam.
*    CLEAR wa_reportparam.
*
*    wa_reportparam-selname = 'S_MONAT'.
*    wa_reportparam-kind    = 'S'.
*    wa_reportparam-sign    = 'I'.
*    wa_reportparam-option  = 'EQ'.
*    wa_reportparam-low = ls_monat-low.
*    APPEND wa_reportparam TO tt_reportparam.
*    CLEAR wa_reportparam.
*
*    LOOP AT lt_aufnr ASSIGNING FIELD-SYMBOL(<fs_aufnr>).
*      wa_reportparam-selname = 'S_AUFNR'.
*      wa_reportparam-kind    = 'S'.
*      wa_reportparam-sign    = 'I'.
*      wa_reportparam-option  = 'EQ'.
*      wa_reportparam-low = <fs_aufnr>-low.
*      APPEND wa_reportparam TO tt_reportparam.
*      CLEAR wa_reportparam.
*    ENDLOOP.
*
*    wa_reportparam-selname = 'S_PROT'.
*    wa_reportparam-kind    = 'S'.
*    wa_reportparam-sign    = 'I'.
*    wa_reportparam-option  = 'EQ'.
*    wa_reportparam-low = iv_job.
*    APPEND wa_reportparam TO tt_reportparam.
*    CLEAR wa_reportparam.
*
*    lv_repname = 'ZFIR0005'.
*    CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
*      EXPORTING
*        iv_reportname         = lv_repname
*        iv_variantname        = iv_varname
*        iv_varianttext        = iv_varianttext
*      IMPORTING
*        ev_funcrc             = wl_subrc
*      TABLES
*        tt_reportparam        = tt_reportparam
*      EXCEPTIONS
*        exist_check_failed    = 1
*        update_failed         = 2
*        update_not_authorized = 3
*        update_no_report      = 4
*        update_no_variant     = 5
*        update_variant_locked = 6
*        insert_failed         = 7
*        insert_not_authorized = 8
*        insert_no_report      = 9
*        insert_variant_exists = 10
*        insert_variant_locked = 11
*        OTHERS                = 12.
*
*    i_head-jobname = iv_job. " Nome do JOB
*    i_head-sdlstrttm = sy-uzeit + 60. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
*    i_head-stepcount = 1.
*    wl_tbtcjob-jobname = iv_job.
*
*    SELECT SINGLE *
*     FROM setleaf
*     INTO @DATA(wl_setleaf)
*      WHERE setname EQ 'MAGGI_JOB_USER'.
*
*    wa_steplist-parameter = iv_varname. " Nome da variante
*    wa_steplist-program = 'ZFIR0005'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
*    wa_steplist-typ = 'A'. " Tipo de Job
*    wa_steplist-authcknam = wl_setleaf-valfrom.
*    wa_steplist-language = sy-langu.
*    wa_steplist-arcuser = wl_setleaf-valfrom.
*
*    APPEND wa_steplist TO i_steplist.
*    CLEAR wa_steplist.
*
*    c_no = 'N'.
*    CALL FUNCTION 'BP_JOB_CREATE'
*      EXPORTING
*        job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
*        job_cr_head_inp     = i_head " os valores atribuidos
*      IMPORTING
*        job_cr_head_out     = wl_tbtcjob
*        job_cr_stdt_out     = wl_tbtcstrt
*      TABLES
*        job_cr_steplist     = i_steplist
*      EXCEPTIONS
*        cant_create_job     = 1
*        invalid_dialog_type = 2
*        invalid_job_data    = 3
*        job_create_canceled = 4
*        OTHERS              = 5.
*
*    IF sy-subrc <> 0.
*      MESSAGE 'Erro ao adicionar passo ao job' TYPE 'E'.
*      CHECK sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    CALL FUNCTION 'JOB_CLOSE'
*      EXPORTING
*        jobcount             = wl_tbtcjob-jobcount
*        jobname              = iv_job
*        strtimmed            = starttimeimmediate
*      EXCEPTIONS
*        cant_start_immediate = 1
*        invalid_startdate    = 2
*        jobname_missing      = 3
*        job_close_failed     = 4
*        job_nosteps          = 5
*        job_notex            = 6
*        lock_failed          = 7
*        invalid_target       = 8
*        OTHERS               = 9.
*
*    IF sy-subrc <> 0.
*      MESSAGE 'Erro ao fechar o job' TYPE 'E'.
*      CHECK sy-subrc <> 0.
*    ELSE.
*
*      IF sy-batch IS INITIAL.
*
*        ls_tvarvc-low  = lv_jobcount.
*        ls_tvarvc-name = iv_job.
*         Verificar se a variável já existe na tabela
*        SELECT *
*        FROM tvarvc
*        INTO TABLE @DATA(dummy)
*        WHERE name = @ls_tvarvc-name.
*
*        IF sy-subrc = 0.
*           Atualizar o registro existente
*          UPDATE tvarvc SET  low = lv_jobcount
*                      WHERE  name = iv_job.
*          ev_ret = abap_true.
*        ELSE.
*           Inserir novo registro
*          INSERT INTO tvarvc VALUES ls_tvarvc.
*          ev_ret = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD GET_DATA.
  ENDMETHOD.


  METHOD GET_FILIAL_MATRIZ.

    DATA: wl_branch_detail TYPE bapibranch.

    CLEAR: wl_branch_detail, es_branch.

    CHECK iv_bukrs IS NOT INITIAL.

    SELECT SINGLE *
      FROM j_1bbranch INTO @DATA(_wl_branch)
     WHERE bukrs  = @iv_bukrs
       AND branch = '0001'. "Matriz

    IF sy-subrc = 0.
      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
        EXPORTING
          company       = _wl_branch-bukrs
          branch        = _wl_branch-branch
        IMPORTING
          branch_detail = wl_branch_detail.

      es_branch-cnpj = wl_branch_detail-cgc_number.
      es_branch-name = wl_branch_detail-name.

      IF wl_branch_detail-adrnr IS NOT INITIAL.
        SELECT SINGLE *
          FROM adrc INTO @DATA(_wl_adrc)
         WHERE addrnumber = @wl_branch_detail-adrnr.

        IF ( sy-subrc = 0 ).
          es_branch-country =  _wl_adrc-country.
          es_branch-region  =  _wl_adrc-region.
          es_branch-street  =  _wl_adrc-street.
          es_branch-city2   =  _wl_adrc-city2.
          es_branch-city1   =  _wl_adrc-city1.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD RECOVERED_JOB.

    DATA: wa_saida TYPE zpme0087.
    CLEAR: wa_saida.

    READ TABLE it_saida INTO wa_saida INDEX 1.
    IF wa_saida-ident IS NOT INITIAL.

      DATA: lv_spoolid    TYPE tsp01-rqident,
            lv_pdf_size   TYPE i,               " PDF size
            lt_pdf_table  TYPE TABLE OF tline,  " Internal table to store PDF
            lv_html       TYPE string,
            lv_base64     TYPE string,
            lv_xstring    TYPE xstring,
            lt_binary     TYPE TABLE OF x255,   " Binary internal table
            lt_lines      TYPE i,
            ev_spoolid    TYPE tsp01-rqident,
            lv_bin_length TYPE i.               " Binary length

      CLEAR:lv_spoolid,lv_base64,lv_pdf_size,lt_pdf_table[],lv_xstring,lt_binary,lv_bin_length.

      lv_spoolid = wa_saida-ident.

      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
        EXPORTING
          src_spoolid              = lv_spoolid   " Spool Request ID
          no_dialog                = 'X'
          dst_device               = 'PDF1'
          pdf_destination          = 'X'
        IMPORTING
          pdf_bytecount            = lv_pdf_size  " PDF Size
          pdf_spoolid              = ev_spoolid
          bin_file                 = lv_xstring
        TABLES
          pdf                      = lt_pdf_table " PDF content
        EXCEPTIONS
          err_no_abap_spooljob     = 1
          err_no_spooljob          = 2
          err_no_permission        = 3
          err_conv_not_possible    = 4
          err_bad_destdevice       = 5
          user_cancelled           = 6
          err_spoolerror           = 7
          err_temseerror           = 8
          err_btcjob_open_failed   = 9
          err_btcjob_submit_failed = 10
          err_btcjob_close_failed  = 11
          OTHERS                   = 12.

      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
        EXPORTING
          input  = lv_xstring
        IMPORTING
          output = lv_base64.

      IF lv_base64 IS NOT INITIAL.
        gv_string_job = lv_base64.
      ENDIF.

*      lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.
*        cl_abap_browser=>show_html( html_string = lv_html title = 'PDF' ).
    ENDIF.
  ENDMETHOD.


  METHOD RUN.

  ENDMETHOD.


  METHOD SET.

*GV_AREA         Instance Attribute Public  Type  KOKRS
*GV_PERIO        Instance Attribute Public  Type  MONAT
*GV_ANO          Instance Attribute Public  Type  GJAHR
*GV_PROTOCOLO    Instance Attribute Public  Type  STRING
*GIT_ORDENS      Instance Attribute Public  Type  ZTTSYS_AUFNRL

    gv_area       = i_area.
    gv_perio      = i_perio.
    gv_ano        = i_ano.
    gv_protocolo  = i_prot.
    git_ordens[]  = it_ordens[].

  ENDMETHOD.


  method ZIF_INTEGRACAO_INBOUND~CONFIGURE_SERVER.

    DATA: lva_reason TYPE STRING,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code is NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input         = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          OUTPUT        = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code         = lva_code
        IMPORTING
          E_DESC_STATUS  = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = conv #( lva_code )
          reason = conv #( lva_reason )
       ).

    endif.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~PROCESSAR_REQUISICAO.

    DATA: lc_integracao TYPE REF TO zcl_integracao.

    CLEAR: e_zintegracao_log.

    r_zif_integracao_inbound = me.

    "Verificar a Função de Cada requisição
    me->zif_integracao_inject~at_info_request_http-ds_funcao_processa  = ''.

    CREATE OBJECT lc_integracao.

    lc_integracao->zif_integracao~set_msg_inject( i_msg = CAST #( me )
      )->set_new_msg( IMPORTING e_id_integracao = me->zif_integracao_inbound~at_id_integracao
      )->set_processar_retorno(
      )->set_integrar_retorno( IMPORTING e_data_retorno = DATA(e_data_retorno) e_zintegracao_log = e_zintegracao_log
      )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
      )->free(
      ).

    me->zif_integracao_inbound~at_zintegracao_log = e_zintegracao_log.

    e_msg = e_data_retorno.
    CLEAR: lc_integracao.

  endmethod.


  method ZIF_INTEGRACAO_INBOUND~SET_DATA.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  endmethod.


  method zif_integracao_inbound~validar_dados_inbound.

    data:
      lit_data_inbound type zime0001,
      ox_err           type ref to cx_root,
      lv_message       type string,
      wa_out           type ty_out,
      wa_sysphera      type zsys_posnr.

    clear: r_msg_erro.

    try.
        if me->zif_integracao_inject~at_info_request_http-ds_metodo ne me->zif_integracao_inject~co_request_method_post   and
           me->zif_integracao_inject~at_info_request_http-ds_metodo ne me->zif_integracao_inject~co_request_method_delete.
          r_msg_erro = 'Metodo informado não reconhecido!'(e01).
          return.
        endif.

        /ui2/cl_json=>deserialize( exporting json = i_data_inbound changing data = lit_data_inbound ).

        if lit_data_inbound is not initial.

*---------------------------------------------------------------------
* Sysphera
*---------------------------------------------------------------------
          loop at lit_data_inbound-id_sysphera[] assigning field-symbol(<fs_sysphera>).
            wa_sysphera-posnr = <fs_sysphera>.

            append wa_sysphera to git_sysphera.
            free wa_sysphera.
          endloop.
        endif.

        if git_sysphera[] is initial."AND gv_protocolo IS INITIAL.
          me->gv_string_job = '"ID Sysphera - informação obrigatória!"'.
          exit.
        endif.

*        select solicitacao_invest from zim01_sol_ap_inv
*          into table @it_found
*          for all entries in @git_sysphera
*          where solicitacao_invest eq @git_sysphera-posnr.

        data: it_found type table of anla,
              wa_found type anla.

        loop at git_sysphera into wa_sysphera.
          wa_found-leanz = wa_sysphera-posnr.
          append wa_found to it_found.
        endloop.

        if it_found  is not initial.
          select anln1, anln2, leanz
            from anla
            into table @it_anla
            for all entries in @it_found
            where leanz eq @it_found-leanz.
        endif.

        if  it_anla is not initial.
          select a~banfn, a~bnfpo, b~anln1, b~anln2 from eban as a
            inner join ebkn as b
            on  a~banfn = b~banfn
            and a~bnfpo = b~bnfpo
            into table @data(it_eban)
            for all entries in @it_anla
            where b~anln1 = @it_anla-anln1
              and b~anln2 = @it_anla-anln2
              and b~loekz = ' '.

          select b~*
            from ekkn as b
            inner join ekpo as p
            on  p~ebeln = b~ebeln
            and p~ebelp = b~ebelp
            and p~loekz = ''
            into table @data(it_ekpo)
            for all entries in @it_anla
            where b~anln1 = @it_anla-anln1
              and b~anln2 = @it_anla-anln2
              and b~loekz = ' '.
        endif.

        if it_ekpo is not initial.
          select * from ekbe
           into table @data(it_ekbe_migo)
           for all entries in @it_ekpo
           where  ebeln  = @it_ekpo-ebeln
              and ebelp  = @it_ekpo-ebelp
              and vgabe  = '1'
              and shkzg  = 'S'
              and not exists ( select * from mseg as e where e~smbln eq ekbe~belnr and e~sjahr eq ekbe~gjahr ).

          select * from ekbe
            into table @data(it_ekbe)
            for all entries in @it_ekpo
            where  ebeln  = @it_ekpo-ebeln
               and ebelp  = @it_ekpo-ebelp
               and vgabe  = '2'
               and shkzg  = 'S'
               and not exists ( select * from rbkp
                                 where rbkp~belnr eq ekbe~belnr
                                 and   rbkp~gjahr eq ekbe~gjahr
                                 and   rbkp~stblg ne @space  ).
        endif.

        loop at it_found assigning field-symbol(<fs_found>).
          read table it_anla into data(wa_anla) with key leanz = <fs_found>-leanz.
          if sy-subrc is not initial.
            " grava id e mensagem
            wa_out-posnr    = <fs_found>-leanz.
            wa_out-mensagem = 'Aguardando imobilizado'.
          else.
            read table it_eban into data(wa_eban) with key anln1 = wa_anla-anln1
                                                           anln2 = wa_anla-anln2.
            if sy-subrc is not initial.
              wa_out-posnr    = <fs_found>-leanz.
              wa_out-mensagem = 'Aguardando requisição'.
              read table it_ekpo into data(wa_ekpo) with key anln1 = wa_anla-anln1
                                                             anln2 = wa_anla-anln2.
              if sy-subrc is not initial.
                wa_out-mensagem = 'Aguardando pedido'.
              endif.
            endif.

            if sy-subrc is not initial.
              " grava id e mensagem
            else.
              loop at it_ekpo into wa_ekpo where anln1 = wa_anla-anln1
                                           and   anln2 = wa_anla-anln2.

                read table it_ekbe_migo into data(wa_ekbe) with key ebeln  = wa_ekpo-ebeln
                                                                    ebelp  = wa_ekpo-ebelp.
                if sy-subrc is not initial.
                  " grava id e mensagem
                  wa_out-posnr    = <fs_found>-leanz.
                  wa_out-mensagem = 'Pedido em andamento, Aguardando Migo'.
                else.
                  read table it_ekbe into data(wa_ekbe2) with key ebeln  = wa_ekpo-ebeln
                                                                  ebelp  = wa_ekpo-ebelp.
                  if sy-subrc is not initial.
                    " grava id e mensagem
                    wa_out-posnr    = <fs_found>-leanz.
                    wa_out-mensagem = 'Pedido Recebido, aguardando pagamento'.
                  else.
                    wa_out-posnr    = <fs_found>-leanz.
                    wa_out-mensagem = 'Pedido Finalizado'.
                  endif.
                  exit.
                endif.
              endloop.
            endif.
          endif.

          append wa_out to me->git_outbound.
          clear wa_out.
        endloop.

        if me->git_outbound is initial.
          loop at git_sysphera assigning field-symbol(<fs_sysp>).
            wa_out-posnr    = <fs_sysp>-posnr.
            wa_out-mensagem = me->gv_string_job.
            append wa_out to me->git_outbound.
            clear wa_out.
          endloop.
        endif.

      catch cx_root into ox_err.
        r_msg_erro = |{ ox_err->get_text( ) }| & |{ 'Erro ZCL_*INV_SYSPHERA!'(e02) }|.
    endtry.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~GET_HEADER_REQUEST_HTTP.

    R_IF_INTEGRACAO_INJECT = ME.
    E_HEADER_FIELDS = ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS.

  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_BEFORE_ERROR_OUTBOUND_MSG.
    E_SUCESSO = ABAP_FALSE.
  endmethod.


  METHOD ZIF_INTEGRACAO_INJECT~SET_BEFORE_SEND_OUTBOUND_MSG.


  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_HEADER_REQUEST_HTTP.
    R_IF_INTEGRACAO_INJECT = ME.
    ME->ZIF_INTEGRACAO_INJECT~AT_HEADER_FIELDS = I_HEADER_FIELDS.
  endmethod.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lit_data_inbound  TYPE zfie0016,
          lit_data_inbounds TYPE string,
          lva_operacao      TYPE string,
          lva_ds_erro       TYPE string.


    r_if_integracao_inject = me.
    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    IF i_msg_inbound IS NOT INITIAL.
      /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
    ENDIF.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).

    IF gv_erro_string IS NOT INITIAL.
      e_msg_erro = e_msg_erro && gv_erro_string.
    ENDIF.

    IF e_msg_erro IS NOT INITIAL.
      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
      RETURN.
    ENDIF.

    DATA(_change_bd) = abap_false.

*    LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

    CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
      WHEN 'POST'. "Inclusão/Modificação

*        SELECT *
*        FROM tvarvc
*        INTO TABLE @DATA(dummy)
*        WHERE name = @me->gv_protocolo.
*
*        IF sy-subrc EQ 0.
        _change_bd = abap_true.
*        ELSE.
*          IF gv_string_job IS INITIAL.
*            e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
*          ENDIF.
*        ENDIF.

      WHEN OTHERS.
        e_msg_erro = 'Operação não prevista!'.
    ENDCASE.

    IF e_msg_erro IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF ( _change_bd = abap_true ) AND ( e_msg_erro IS INITIAL ).

*      IF me->gv_string_job IS NOT INITIAL.
*        DATA: lv_data TYPE sy-datum,
*              lv_job  TYPE btcjob.
*        lv_data = sy-datum - 7.
*        lv_job = gv_protocolo.
*        DELETE FROM zfit0020 WHERE data LE lv_data.
*
*        DELETE FROM zfit0020 WHERE jobname EQ lv_job.
*        COMMIT WORK.
*      ENDIF.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.

      IF me->git_outbound IS NOT INITIAL.


        e_msg_outbound = /ui2/cl_json=>serialize( EXPORTING data        = me->git_outbound
                                                   pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

*        LOOP AT me->git_outbound ASSIGNING FIELD-SYMBOL(<fs_sys>).
*          e_msg_outbound = ' { "Id_Sysphera" : "'   && <fs_sys>-posnr   &&  '" ,'   && cl_abap_char_utilities=>newline &&
*                           '   "Ret_Message" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
*                           ' }'.
*        ENDLOOP.
      ELSE.

        e_msg_erro = me->gv_string_job.
        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.

*        e_msg_outbound = ' { "Id_Sysphera" : "'   && me->gv_protocolo   &&  '" ,'   && cl_abap_char_utilities=>newline &&
*                         '   "Ret_Message" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
*                         ' }'.

      ENDIF.
    ELSE.

*      IF gv_string_job IS NOT INITIAL.
*        e_sucesso   = abap_true.
*        e_nm_code   = '200'.
*        e_msg_erro  = 'Ok'.
*        e_msg_outbound = ' { "Protocolo" : "'   && me->gv_protocolo   &&  '" ,'   && cl_abap_char_utilities=>newline &&
*                         '   "Status_code" : "' && e_nm_code          &&  '" ,'  && cl_abap_char_utilities=>newline &&
*                         '   "Resultado" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
*                         ' }'.
*      ELSE.
      ROLLBACK WORK.

      e_sucesso      = abap_true.
      e_nm_code      = '400'.
      e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                       '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                       '}'.
*      ENDIF.
    ENDIF.
  ENDMETHOD.


  method ZIF_INTEGRACAO_INJECT~SET_INTEGRAR_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PARAMETRO.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_INBOUND.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.


  method ZIF_INTEGRACAO_INJECT~SET_PROCESSA_RETORNO.
    R_IF_INTEGRACAO_INJECT = ME.
    E_SUCESSO = ABAP_TRUE.
  endmethod.
ENDCLASS.
