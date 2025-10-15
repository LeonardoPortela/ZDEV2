class ZCL_INT_IB_FI_PROT_MCOMP_INV definition
  public
  final
  create public .

public section.

  interfaces ZIF_INTEGRACAO_INJECT .
  interfaces ZIF_INTEGRACAO_INBOUND .

  types:
    BEGIN OF ty_branch,
        cnpj    TYPE bapibranch-cgc_number,
        name    TYPE bapibranch-name,
        country TYPE adrc-country,
        region  TYPE adrc-region,
        street  TYPE adrc-street,
        city2   TYPE adrc-city2,
        city1   TYPE adrc-city1.
    TYPES  END OF ty_branch .
  types:
    BEGIN OF ty_zsdt0170,
        include TYPE zsdt0170.
    TYPES  END OF ty_zsdt0170 .
  types:
    tyt_zsdt0170 TYPE TABLE OF ty_zsdt0170 .
  types:
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
  types:
    tyt_conf TYPE TABLE OF zfit0012 .

  data GIT_DATA_INBOUND type ZFIE0016 .
  data GV_AREA type KOKRS .
  data GV_PERIO type MONAT .
  data GV_ANO type GJAHR .
  data GV_PROTOCOLO type STRING .
  data GV_PROT type STRING .
  data GIT_ORDENS type ZTTSYS_AUFNRL .
  data IT_CONF type TYT_CONF .
  data:
    it_saida TYPE STANDARD TABLE OF zpme0087 .
  data AT_ID_INTERFACE type ZDE_ID_INTERFACE .
  data AT_ID_DUE type NUM10 .
  data GV_STRING_JOB type STRING .
  data GV_ERRO_STRING type STRING .

  methods CONSTRUCTOR
    raising
      ZCX_INTEGRACAO .
  methods GET_FILIAL_MATRIZ
    importing
      !IV_BUKRS type BUKRS
    exporting
      !ES_BRANCH type TY_BRANCH .
  methods CREATE_JOB
    importing
      !IV_JOB type BTCJOB
    exporting
      !EV_RET type CHAR1 .
  methods RUN
    exporting
      !LT_MESSAGE type STRING
      !IT_CONF type TYT_CONF .
  methods SET
    importing
      value(I_AREA) type KOKRS
      value(I_PERIO) type MONAT
      value(I_ANO) type GJAHR
      !I_PROT type STRING optional
      value(IT_ORDENS) type ZTTSYS_AUFNRL .
  methods GET_DATA .
  methods RECOVERED_JOB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INT_IB_FI_PROT_MCOMP_INV IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->zif_integracao_inject~at_id_interface    = ME->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD create_job.

    TYPES:
      BEGIN OF ty_kokrs,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE kokrs,
        high   TYPE kokrs,
      END OF ty_kokrs .

    TYPES:
      BEGIN OF ty_monat,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE monat,
        high   TYPE monat,
      END OF ty_monat .

    TYPES:
      BEGIN OF ty_gjahr,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE gjahr,
        high   TYPE gjahr,
      END OF ty_gjahr .

    TYPES:
      BEGIN OF ty_aufnr,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE aufnr,
        high   TYPE aufnr,
      END OF ty_aufnr .

    DATA:
      ls_kokrs TYPE ty_kokrs,
      ls_monat TYPE ty_monat,
      ls_gjahr TYPE ty_gjahr,
      ls_aufnr TYPE ty_aufnr,
      lt_kokrs TYPE TABLE OF ty_kokrs,
      lt_monat TYPE TABLE OF ty_monat,
      lt_gjahr TYPE TABLE OF ty_gjahr,
      lt_aufnr TYPE TABLE OF ty_aufnr.

    DATA:
      lv_jobcount         TYPE tbtcjob-jobcount,               " Identificador do job
      lv_startdate        TYPE d,
      lv_starttime        TYPE t,
      lv_enddate          TYPE d,
      lv_endtime          TYPE t,
      ls_tvarvc           TYPE tvarvc,
      ls_spool_parameters TYPE pri_params.

    DATA: lv_ano            TYPE numc4,
          lv_mes            TYPE numc2,
          lv_ultimo_dia_mes TYPE endda,
          lv_qtd_dias       TYPE tbtcjob-prddays.

    DATA: lv_repname TYPE  rsvar-report.           " for variant handling
    DATA: iv_varname TYPE  raldb-variant VALUE 'SAP_UPG_V1'.
    DATA: iv_varianttext  TYPE  varit-vtext VALUE 'Upgrade variant'.
    DATA: wl_subrc TYPE sy-subrc.
    DATA: tt_reportparam TYPE TABLE OF  rsparams,
          wa_reportparam TYPE rsparams.

    DATA : c_no(1) TYPE c . "value 'N', " Criação do job
    DATA: i_steplist  TYPE STANDARD TABLE OF tbtcstep,
          wa_steplist TYPE tbtcstep.
    DATA: wl_tbtcjob  TYPE  tbtcjob,
          wl_tbtcstrt TYPE  tbtcstrt,
          i_head      TYPE tbtcjob.

    DATA: starttimeimmediate TYPE btch0000-char1 VALUE 'X',
          lv_sdlstrtdt       TYPE btcsdate,
          lv_sdlstrttm       TYPE btcstime.


    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_area  high = '' ) TO lt_kokrs[].
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_perio high = '' ) TO lt_monat[].
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gv_ano   high = '' ) TO lt_gjahr[].

    ls_kokrs-sign   = 'I'.
    ls_kokrs-option = 'EQ'.
    ls_kokrs-low    = gv_area.
    ls_monat = ls_kokrs.
    ls_gjahr = ls_kokrs.
    ls_aufnr = ls_kokrs.
    ls_monat-low = gv_perio.
    ls_gjahr-low = gv_ano.

    LOOP AT git_ordens ASSIGNING FIELD-SYMBOL(<fs_ord>).
      IF sy-tabix EQ 1.
        ls_aufnr-low = <fs_ord>.
      ENDIF.
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ord> high = '' ) TO lt_aufnr[].
    ENDLOOP.

    wa_reportparam-selname = 'S_KOKRS'.
    wa_reportparam-kind    = 'S'.
    wa_reportparam-sign    = 'I'.
    wa_reportparam-option  = 'EQ'.
    wa_reportparam-low = ls_kokrs-low.
    APPEND wa_reportparam TO tt_reportparam.
    CLEAR wa_reportparam.

    wa_reportparam-selname = 'S_GJAHR'.
    wa_reportparam-kind    = 'S'.
    wa_reportparam-sign    = 'I'.
    wa_reportparam-option  = 'EQ'.
    wa_reportparam-low = ls_gjahr-low.
    APPEND wa_reportparam TO tt_reportparam.
    CLEAR wa_reportparam.

    wa_reportparam-selname = 'S_MONAT'.
    wa_reportparam-kind    = 'S'.
    wa_reportparam-sign    = 'I'.
    wa_reportparam-option  = 'EQ'.
    wa_reportparam-low = ls_monat-low.
    APPEND wa_reportparam TO tt_reportparam.
    CLEAR wa_reportparam.

    LOOP AT lt_aufnr ASSIGNING FIELD-SYMBOL(<fs_aufnr>).
      wa_reportparam-selname = 'S_AUFNR'.
      wa_reportparam-kind    = 'S'.
      wa_reportparam-sign    = 'I'.
      wa_reportparam-option  = 'EQ'.
      wa_reportparam-low = <fs_aufnr>-low.
      APPEND wa_reportparam TO tt_reportparam.
      CLEAR wa_reportparam.
    ENDLOOP.

    wa_reportparam-selname = 'S_PROT'.
    wa_reportparam-kind    = 'S'.
    wa_reportparam-sign    = 'I'.
    wa_reportparam-option  = 'EQ'.
    wa_reportparam-low = iv_job.
    APPEND wa_reportparam TO tt_reportparam.
    CLEAR wa_reportparam.

    lv_repname = 'ZFIR0005'.
    CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
      EXPORTING
        iv_reportname         = lv_repname
        iv_variantname        = iv_varname
        iv_varianttext        = iv_varianttext
      IMPORTING
        ev_funcrc             = wl_subrc
      TABLES
        tt_reportparam        = tt_reportparam
      EXCEPTIONS
        exist_check_failed    = 1
        update_failed         = 2
        update_not_authorized = 3
        update_no_report      = 4
        update_no_variant     = 5
        update_variant_locked = 6
        insert_failed         = 7
        insert_not_authorized = 8
        insert_no_report      = 9
        insert_variant_exists = 10
        insert_variant_locked = 11
        OTHERS                = 12.

    i_head-jobname = iv_job. " Nome do JOB
    i_head-sdlstrttm = sy-uzeit + 60. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
    i_head-stepcount = 1.
    wl_tbtcjob-jobname = iv_job.

    SELECT SINGLE *
     FROM setleaf
     INTO @DATA(wl_setleaf)
      WHERE setname EQ 'MAGGI_JOB_USER'.

    wa_steplist-parameter = iv_varname. " Nome da variante
    wa_steplist-program = 'ZFIR0005'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
    wa_steplist-typ = 'A'. " Tipo de Job
    wa_steplist-authcknam = wl_setleaf-valfrom.
    wa_steplist-language = sy-langu.
    wa_steplist-arcuser = wl_setleaf-valfrom.

    APPEND wa_steplist TO i_steplist.
    CLEAR wa_steplist.

    c_no = 'N'.
    CALL FUNCTION 'BP_JOB_CREATE'
      EXPORTING
        job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
        job_cr_head_inp     = i_head " os valores atribuidos
      IMPORTING
        job_cr_head_out     = wl_tbtcjob
        job_cr_stdt_out     = wl_tbtcstrt
      TABLES
        job_cr_steplist     = i_steplist
      EXCEPTIONS
        cant_create_job     = 1
        invalid_dialog_type = 2
        invalid_job_data    = 3
        job_create_canceled = 4
        OTHERS              = 5.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao adicionar passo ao job' TYPE 'E'.
      CHECK sy-subrc <> 0.
*      EXIT.
    ENDIF.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = wl_tbtcjob-jobcount
        jobname              = iv_job
        strtimmed            = starttimeimmediate
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao fechar o job' TYPE 'E'.
      CHECK sy-subrc <> 0.
    ELSE.

      IF sy-batch IS INITIAL.

        ls_tvarvc-low  = lv_jobcount.
        ls_tvarvc-name = iv_job.
        " Verificar se a variável já existe na tabela
        SELECT *
        FROM tvarvc
        INTO TABLE @DATA(dummy)
        WHERE name = @ls_tvarvc-name.

        IF sy-subrc = 0.
          " Atualizar o registro existente
          UPDATE tvarvc SET  low = lv_jobcount
                      WHERE  name = iv_job.
          ev_ret = abap_true.
        ELSE.
          " Inserir novo registro
          INSERT INTO tvarvc VALUES ls_tvarvc.
          ev_ret = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_data.

    DATA nmjob TYPE tbtco-jobname.

    nmjob = gv_protocolo.
    FREE: it_saida.
    SELECT
       substring( a~jobname,1,4 ) AS empresa "area
       ,a~jobname
       ,a~jobcount
       ,CASE WHEN a~reluname <> ' ' THEN a~reluname ELSE a~sdluname END AS executor_nm
       ,a~reldate AS dtstart
       ,a~reltime AS hrstart
       ,a~enddate AS dtfinish
       ,a~endtime AS hrfinish
       ,CASE
       WHEN a~status = 'A' THEN '@0A@'
       WHEN a~status = 'F' THEN '@08@'
       ELSE '@09@'
       END AS job_status
       ,CASE
       WHEN a~status = 'A' THEN 'Cancelado'
       WHEN a~status = 'F' THEN 'Completado'
       WHEN a~status = 'P' THEN 'Agendado'
       WHEN a~status = 'R' THEN 'Ativo'
       WHEN a~status = 'S' THEN 'Lançado'
       END AS job_desc
       ,' ' AS liq_status
       ,CAST( b~listident AS INT4 ) AS ident
       FROM tbtco AS a
       LEFT JOIN tbtcp AS b ON a~jobname = b~jobname
       AND a~jobcount = b~jobcount
*   where substring( a~jobname,1,8 ) = nmjob
   WHERE a~jobname = @nmjob
   AND a~reldate > 0
   INTO TABLE @it_saida.

    SORT it_saida BY jobname ASCENDING dtstart DESCENDING hrstart DESCENDING.

    DELETE ADJACENT DUPLICATES FROM it_saida COMPARING jobname.

    DATA ident TYPE i.
    DATA lt_spool TYPE STANDARD TABLE OF lvc_s_1022 INITIAL SIZE 0 .
    DATA l_file_length  TYPE rspoid.
    DATA spool_contents TYPE soli_tab.
    DATA spool_contstr  TYPE string.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_get_spoll>).
      CLEAR: l_file_length,ident.
      FREE: lt_spool.

      IF <_get_spoll>-ident > 0.

        "Verificar se a ordem spool ainda esta disponivel.
        CALL FUNCTION 'RSPO_CHECK_JOB_ID_PERMISSION'
          EXPORTING
            rqident       = <_get_spoll>-ident
            access        = 'DISP'
          EXCEPTIONS
            no_such_job   = 1
            no_permission = 2
            OTHERS        = 3.
        IF sy-subrc EQ 0.

          CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
            EXPORTING
              rqident = <_get_spoll>-ident
            TABLES
              buffer  = spool_contents.
          IF sy-subrc = 0.

            DATA: it_char255 TYPE TABLE OF char255,
                  wa_char255 TYPE char255.
            DATA: it_string TYPE TABLE OF string,
                  wa_string TYPE string.


            LOOP AT spool_contents INTO DATA(wa_cont).
              IF wa_cont CS '----------------------------------------------------------------------------'
               OR wa_cont CS 'Programa Mapa de Comprovação de Investimentos'.
                CONTINUE.
              ENDIF.
              wa_char255 = wa_cont.
              APPEND wa_char255 TO it_char255.
              CLEAR wa_char255.
            ENDLOOP.

            DATA: lv_index TYPE sy-tabix.

            LOOP AT it_char255 INTO wa_char255.
              me->gv_string_job = me->gv_string_job && wa_char255.
            ENDLOOP.

*            LOOP AT spool_contents INTO DATA(lr_spool).
*              IF sy-tabix = 22 .
*                <_get_spoll>-liq_result = lr_spool.
*                CONDENSE lr_spool NO-GAPS.
*
*                CLEAR: <_get_spoll>-liq_status.
*                CASE lr_spool.
*                  WHEN 'Processamentoencerradocomerros'.
*                    <_get_spoll>-liq_status = '@0A@'.
*                  WHEN OTHERS.
*                    <_get_spoll>-liq_status = '@08@'.
*                ENDCASE.
*              ENDIF.
*            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF spool_contents[] IS INITIAL.

      READ TABLE it_saida INTO DATA(wa_saida) INDEX 1.
      IF sy-subrc IS INITIAL.
        IF wa_saida-job_desc NE 'Completado'.
          IF wa_saida-job_desc EQ 'Cancelado'.
            me->gv_string_job = '"Erro no processamento!"'.
          ELSE.
            me->gv_string_job = '"Informações ainda não disponíveis!"'.
          ENDIF.
        ELSE.
          SELECT SINGLE * FROM zfit0020
            INTO @DATA(wa_fit0020)
            WHERE jobname EQ @nmjob.
          IF sy-subrc IS INITIAL AND wa_fit0020-info IS NOT INITIAL.
            me->gv_string_job = wa_fit0020-info.
          ELSE.
            me->gv_string_job = '"Não foram encontrados dados para os parametros informados!"'.
          ENDIF.
        ENDIF.
      ELSE.
        me->gv_string_job = '"Protocolo não encontrado!"'.
      ENDIF.
    ELSE.
      FREE:spool_contents.
    ENDIF.

*    SORT it_saida BY liq_status.
*    DELETE it_saida WHERE liq_status EQ space.

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


  METHOD recovered_job.

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


  method run.

    data: it_result_kob1   type table of zsys_kob1,
          it_result_aux    type table of zsys_kob1,
          it_result_fbl3na type table of zsys_fbl3n_adt,
          it_result_fbl3nx type table of zsys_fbl3n_adt,
          it_bw_fi_05      type table of zdados_saida,
          it_ordensx       type zttsys_aufnr,
          it_empresax      type table of fin_cfin_s_apar_burks,
          it_select        type table of rsselect.

    if git_ordens[] is not initial.

      loop at git_ordens assigning field-symbol(<fs_ord>).
        append initial line to it_ordensx assigning field-symbol(<fs_ordx>).
        <fs_ordx>-aufnr = <fs_ord>.
      endloop.

      call function 'Z_KOB1_SYSPHERA'
        exporting
          i_area    = gv_area
          i_periodo = gv_perio
          i_ano     = gv_ano
          it_ordens = it_ordensx
        tables
          resultado = it_result_aux
          pedidos   = it_result_kob1. "ebeln
    endif.

    if it_result_kob1[] is not initial.
      sort it_result_kob1 by ebeln.
      delete it_result_kob1 where ebeln is initial.
    endif.

    if it_result_kob1[] is not initial.
      select po~bukrs, po~werks, ko~ebeln from ekko as ko
        inner join ekpo as po
        on ko~ebeln eq po~ebeln
        into table @data(it_ekkopo)
        for all entries in @it_result_kob1
        where ko~ebeln eq @it_result_kob1-ebeln.
      if sy-subrc is initial.
        sort it_ekkopo by bukrs ebeln.

        data(it_ek) = it_ekkopo[].
        sort it_ek by bukrs.

        delete adjacent duplicates from it_ek comparing bukrs.

      endif.

    endif.

*---------------------------------------------------------------------
*2) Z_FBL3NA_SYSPHERA ( obter pedidos FM existente) - Usar a lógica
*---------------------------------------------------------------------
    loop at it_ek assigning field-symbol(<fs_ek>).
      append initial line to it_empresax assigning field-symbol(<fs_empx>).
      <fs_empx>-bukrs = <fs_ek>-bukrs.
    endloop.

    loop at it_ek assigning <fs_ek>.
      call function 'Z_FBL3NA_SYSPHERA'
        exporting
          i_periodo  = gv_perio
          i_ano      = gv_ano
        tables
          it_empresa = it_empresax
          resultado  = it_result_fbl3nx. "xblnr

      loop at it_result_fbl3nx assigning field-symbol(<fs_result>).
        append initial line to it_result_fbl3na assigning field-symbol(<fs_result1>).
        <fs_result1> = corresponding #( <fs_result> ).
      endloop.

    endloop.

    if it_result_fbl3na[] is not initial.
      sort it_result_fbl3na by ebeln.
      delete it_result_fbl3na where ebeln is initial.
    endif.

    if it_result_fbl3na[] is not initial.
      select po~bukrs, po~werks, ko~ebeln, ko~budg_type from ekko as ko
        inner join ekpo as po
        on ko~ebeln eq po~ebeln
        into table @data(it_ekkopox)
        for all entries in @it_result_fbl3na
        where po~bukrs    eq @it_result_fbl3na-bukrs
          and ko~ebeln eq @it_result_fbl3na-ebeln.
      if sy-subrc is initial.
        sort it_ekkopox by ebeln.

        loop at it_ekkopox assigning field-symbol(<fs_ekkopox>).
          clear <fs_ekkopox>-budg_type.
          read table it_ekkopo into data(wa_ekkopo) with key bukrs = <fs_ekkopox>-bukrs
                                                             werks = <fs_ekkopox>-werks.
          if sy-subrc is not initial.
            <fs_ekkopox>-budg_type = 'XX'.
          endif.
        endloop.

        delete it_ekkopox where budg_type = 'XX'.

        loop at it_ekkopox assigning <fs_ekkopox>.
          append initial line to it_ekkopo assigning field-symbol(<fs_ekkopo>).
          <fs_ekkopo> = corresponding #( <fs_ekkopox> ).
        endloop.

      endif.
    endif.

    if it_result_fbl3na is not initial or it_result_kob1 is not initial.

      loop at it_ekkopo assigning <fs_ekkopo>.
        append value #( fieldnm = 'bukrs'  sign = 'I' option = 'EQ' low = <fs_ekkopo>-bukrs ) to it_select[].
        append value #( fieldnm = 'branch' sign = 'I' option = 'EQ' low = <fs_ekkopo>-werks ) to it_select[].
      endloop.

      if it_select[] is not initial.

        sort it_select by fieldnm low.
        delete adjacent duplicates from it_select[] comparing fieldnm low.

        data: lv_dat  type sy-datum,
              lv_udia type sy-datum.
        lv_dat = gv_ano && gv_perio && '01'.

        call function 'RP_LAST_DAY_OF_MONTHS'
          exporting
            day_in            = lv_dat
          importing
            last_day_of_month = lv_udia.

        append value #( fieldnm = 'direct'  sign = 'I' option = 'EQ' low = '1' ) to it_select[].
        append value #( fieldnm = 'pstdat'  sign = 'I' option = 'BT' low = lv_dat high = lv_udia ) to it_select[].
        append value #( fieldnm = 'tmptod'  sign = 'I' option = 'EQ' low = abap_on ) to it_select[].
        append value #( fieldnm = 'model'   sign = 'I' option = 'NE' low = '58' high = '58' ) to it_select[].

      endif.
*---------------------------------------------------------------------
*3) ZF_BW_FI_05  -( ( obter notas para retorno da API - FM existente a maioria dos campos retorno
*---------------------------------------------------------------------
      call function 'ZF_BW_FI_05'
        exporting
          p_ativas                     = abap_on
          p_autor                      = abap_on
        tables
          i_t_select                   = it_select
          e_t_data                     = it_bw_fi_05
        exceptions
          no_more_data                 = 1
          error_passed_to_mess_handler = 2
          others                       = 3.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

*     lines_1 = lines( it_bw_fi_05 ).
*     MESSAGE |Metodo Run - ZCL_INT_PROT - Step 4 - { lines_1 } | TYPE 'S'.

    endif.

    select chave_nfe, prod_cfop, icms_cst
      from zib_nfe_dist_itm
      into table @data(it_nfe_dist_itm)
      for all entries in @it_bw_fi_05
      where chave_nfe eq @it_bw_fi_05-chave_nfe.
    if sy-subrc is initial.
      sort it_nfe_dist_itm by chave_nfe.
    endif.

    sort it_ekkopo by bukrs ebeln.
    if it_ekkopo[] is not initial.
      select ebeln, ebelp, matnr
        into table @data(it_ekpo)
        from ekpo
        for all entries in @it_ekkopo
        where ebeln = @it_ekkopo-ebeln.
      if it_ekpo[] is not initial.
        select *
         into table @data(it_ekkn)
         from ekkn
         for all entries in @it_ekpo
         where ebeln = @it_ekpo-ebeln
         and   ebelp = @it_ekpo-ebelp.
      endif.
    endif.
    sort it_ekpo by ebeln matnr.
    sort it_ekkn by ebeln ebelp.
* Trabalhando com a conf
    data vg_objnr type imakz-objnr.
    loop at it_bw_fi_05 assigning field-symbol(<fs_fi_05>).

* Eliminar todos os registros que não estiverem com os pedidos acima.
      read table it_ekkopo assigning <fs_ekkopo> with key bukrs = <fs_fi_05>-bukrs
                                                          ebeln = <fs_fi_05>-ebeln
                                                               binary search.
      check sy-subrc is initial.

      append initial line to it_conf assigning field-symbol(<fs_conf>).
      <fs_conf> = corresponding #( <fs_fi_05> ).
      read table it_ekpo into data(wa_ekpo) with key ebeln = <fs_fi_05>-ebeln
                                                     matnr = <fs_fi_05>-produto binary search.
      if sy-subrc = 0.
        read table it_ekkn into data(wa_ekkn) with key ebeln = wa_ekpo-ebeln
                                                       ebelp = wa_ekpo-ebelp binary search.
        if sy-subrc = 0.
          <fs_conf>-ordem = wa_ekkn-aufnr.

          select single leanz  into @data(_leanz)
              from anla
               where bukrs eq @<fs_fi_05>-bukrs
               and   anln1 eq @wa_ekkn-anln1.
          if _leanz is not initial.
            <fs_conf>-solicitacao_invest = _leanz.
          else.
            concatenate 'OR' wa_ekkn-aufnr into vg_objnr .
            select single posnr into @data(_posnr)
              from imakz
              where objnr = @vg_objnr.
            if sy-subrc = 0.
              <fs_conf>-solicitacao_invest = _posnr.
*              select single solicitacao_invest into <fs_conf>-solicitacao_invest
*               from zim01_sol_ap_inv
*               where posnr eq _posnr.
            endif.
          endif.
        endif.

      endif.

      read table it_nfe_dist_itm assigning field-symbol(<fs_dist_itm>) with key chave_nfe = <fs_fi_05>-chave_nfe
                                                                       binary search.
      if sy-subrc is initial.
        <fs_conf>-prod_cfop = <fs_dist_itm>-prod_cfop.
        <fs_conf>-icms_cst  = <fs_dist_itm>-icms_cst.
      endif.

    endloop.

    data: lv_ultimo type sy-tabix,
          lv_index  type sy-tabix.
    data lv_info type string.

    if it_conf[] is not initial.
      lv_ultimo = lines( it_conf ).
      lv_info = '['.
      loop at it_conf assigning <fs_conf>.
        select single maktx into @data(_maktx) from makt where  matnr = @<fs_conf>-produto and spras = 'P'.
        lv_index = lv_index + 1.
        lv_info =  lv_info && '{' && '"chavenfe": "' && <fs_conf>-chave_nfe && '",'.
        lv_info =  lv_info && '"Produto": "' && <fs_conf>-produto && '",'.
        lv_info =  lv_info && '"Descrição": "' && _maktx && '",'.
        lv_info =  lv_info && '"cod_clifor": "' && <fs_conf>-cod_clifor && '",'.
        lv_info =  lv_info && '"nome_clifor": "' && <fs_conf>-nome_clifor && '",'.
        lv_info =  lv_info && '"cpf_prod": "' && <fs_conf>-cpf_prod && '",'.
        lv_info =  lv_info && '"nfenum": "' && <fs_conf>-nfenum && '-' && <fs_conf>-series && '",'.
        lv_info =  lv_info && '"docdat": "' && <fs_conf>-docdat && '",'.
        lv_info =  lv_info && '"netwrt": "' && <fs_conf>-netwrt && '",'.
        lv_info =  lv_info && '"prod_cfop": "' && <fs_conf>-prod_cfop && '",'.
        lv_info =  lv_info && '"icms_cst": "' && <fs_conf>-icms_cst && '",'.
        lv_info =  lv_info && '"pstdat": "' && <fs_conf>-pstdat && '",'.
        lv_info =  lv_info && '"ordem": "' && <fs_conf>-ordem && '",'.
        lv_info =  lv_info && '"solicitacao_invest": "' && <fs_conf>-solicitacao_invest && '"'.
        if lv_index ne lv_ultimo.
          lv_info =  lv_info && '},'.
        else.
          lv_info =  lv_info && '}'.
        endif.
      endloop.
      lv_info =  lv_info && ']'.

      if lv_info is not initial.

*ZFIT0020
        data: lv_prot     type tbtcjob-jobname,
              wa_zfit0020 type zfit0020.

        lv_prot = gv_protocolo.

        wa_zfit0020-jobname  = lv_prot.
        wa_zfit0020-usuario = sy-uname.
        wa_zfit0020-data = sy-datum.
        wa_zfit0020-hora = sy-uzeit.
        wa_zfit0020-info = lv_info.

        insert zfit0020 from wa_zfit0020.
        if sy-subrc is initial.
          commit work.
        else.
          rollback work.
        endif.
      endif.
    endif.

  endmethod.


  METHOD set.

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


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA:
      lit_data_inbound TYPE zfie0016,
      lv_prog_exec     TYPE sy-repid  VALUE 'ZFIR0005',
      lv_jobname       TYPE tbtcjob-jobname,  " Nome do job
      ox_err           TYPE REF TO cx_root,
      it_conf          TYPE TABLE OF zfit0012,
      lv_message       TYPE string,
      wa_ordens        TYPE zsys_aufnr.

    CONSTANTS: c_magi(4) TYPE c VALUE 'MAGI'.

    CLEAR: r_msg_erro.

    TRY.
        IF me->zif_integracao_inject~at_info_request_http-ds_metodo NE me->zif_integracao_inject~co_request_method_post   AND
           me->zif_integracao_inject~at_info_request_http-ds_metodo NE me->zif_integracao_inject~co_request_method_delete.
          r_msg_erro = 'Metodo informado não reconhecido!'(e01).
          RETURN.
        ENDIF.

        /ui2/cl_json=>deserialize( EXPORTING json = i_data_inbound CHANGING data = lit_data_inbound ).

        IF lit_data_inbound IS NOT INITIAL.

*---------------------------------------------------------------------
* Header
*---------------------------------------------------------------------
          IF lit_data_inbound-area IS INITIAL.
            gv_area      = c_magi.
          ELSE.
            gv_area      = lit_data_inbound-area.
          ENDIF.
          gv_perio       = lit_data_inbound-perio.
          gv_ano         = lit_data_inbound-ano.
          gv_protocolo   = lit_data_inbound-protocolo.

*---------------------------------------------------------------------
* Ordens
*---------------------------------------------------------------------
          LOOP AT lit_data_inbound-it_ordens[] ASSIGNING FIELD-SYMBOL(<fs_ordens>).
            wa_ordens-aufnr = <fs_ordens>.

            APPEND wa_ordens TO git_ordens.
            FREE wa_ordens.
          ENDLOOP.
        ENDIF.

        IF git_ordens[] IS INITIAL AND gv_protocolo IS INITIAL.
          me->gv_string_job = '"Nº ordem informação obrigatória!"'.
          EXIT.
        ENDIF.

        IF gv_protocolo IS INITIAL.
*---------------------------------------------------------------------
* Processo principal - Rodar e retornar protocolo
*---------------------------------------------------------------------

          FREE:lv_jobname.
          lv_jobname = |{ gv_area && '_' && gv_perio && gv_ano && '_' && sy-datum && sy-uzeit && '229' }| .
          gv_protocolo = lv_jobname.
*---------------------------------------------------------------------
* Job criar e seguir
*---------------------------------------------------------------------
          me->create_job( EXPORTING iv_job = lv_jobname
                          IMPORTING ev_ret = DATA(lv_ret) ).
*---------------------------------------------------------------------
* criar protocolo para retorno
*---------------------------------------------------------------------

*          IF sy-subrc is initial.
          IF lv_ret IS NOT INITIAL.
            gv_protocolo = lv_jobname.
          ENDIF.
        ELSE.

* Rodar e procurar retorno caso não encontre avisar via msg que não encontrado, soliciar novamente.
* Retorno via protocol
* Busca job ou via tabela analisar
* Verificar se a variável já existe na tabela
          SELECT *
          FROM tvarvc
          INTO TABLE @DATA(dummy)
          WHERE name = @gv_protocolo.

*---------------------------------------------------------------------
* Busca Informações
*---------------------------------------------------------------------
          IF sy-subrc = 0.
            " Job gerado
            me->get_data( ).
*            me->recovered_job( ).
          ELSE.
            me->gv_string_job = '"Protocolo não encontrado!"'.
          ENDIF.

        ENDIF.
      CATCH cx_root INTO ox_err.
        r_msg_erro = |{ ox_err->get_text( ) }| & |{ 'Erro ZCL_*PROT_MCOMP_INV!'(e02) }|.
    ENDTRY.

  ENDMETHOD.


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

        SELECT *
        FROM tvarvc
        INTO TABLE @DATA(dummy)
        WHERE name = @me->gv_protocolo.

        IF sy-subrc EQ 0.
          _change_bd = abap_true.
        ELSE.
          IF gv_string_job IS INITIAL.
            e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
          ENDIF.
        ENDIF.

      WHEN OTHERS.
        e_msg_erro = 'Operação não prevista!'.
    ENDCASE.

    IF e_msg_erro IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF ( _change_bd = abap_true ) AND ( e_msg_erro IS INITIAL ).

      IF me->gv_string_job IS NOT INITIAL.
        DATA: lv_data TYPE sy-datum,
              lv_job  TYPE btcjob.
        lv_data = sy-datum - 7.
        lv_job = gv_protocolo.
        DELETE FROM zfit0020 WHERE data LE lv_data.

        DELETE FROM zfit0020 WHERE jobname EQ lv_job.
        COMMIT WORK.
      ENDIF.
      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.

      IF me->gv_string_job IS NOT INITIAL.
        e_msg_outbound = ' { "Protocolo" : "'   && me->gv_protocolo   &&  '" ,'   && cl_abap_char_utilities=>newline &&
                         '   "Status_code" : "' && e_nm_code          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Resultado" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
                         ' }'.
      ELSE.
        e_msg_outbound = ' { "Status_code" : "'   && e_nm_code   &&  '" ,'   && cl_abap_char_utilities=>newline &&
                         '   "Area" : "' && me->gv_area          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Periodo" : "' && me->gv_perio          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Ano" : "' && me->gv_ano          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Protocolo" : "'   && me->gv_protocolo  &&  '" '  && cl_abap_char_utilities=>newline &&
                         ' }'.

      ENDIF.
    ELSE.

      IF gv_string_job IS NOT INITIAL.
        e_sucesso   = abap_true.
        e_nm_code   = '200'.
        e_msg_erro  = 'Ok'.
        e_msg_outbound = ' { "Protocolo" : "'   && me->gv_protocolo   &&  '" ,'   && cl_abap_char_utilities=>newline &&
                         '   "Status_code" : "' && e_nm_code          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Resultado" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
                         ' }'.
      ELSE.
        ROLLBACK WORK.

        e_sucesso      = abap_true.
        e_nm_code      = '400'.
        e_msg_outbound = '{  "error": "'        && e_msg_erro && '" ,' && cl_abap_char_utilities=>newline &&
                         '   "status_code" : "' && e_nm_code  && '" '  && cl_abap_char_utilities=>newline &&
                         '}'.
      ENDIF.
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
