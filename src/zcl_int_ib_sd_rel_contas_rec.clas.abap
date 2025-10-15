CLASS zcl_int_ib_sd_rel_contas_rec DEFINITION
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
      BEGIN OF ty_saida,
        bukrs              TYPE t001-bukrs,
        zid_lanc           TYPE zfit0026-zid_lanc,
        werks              TYPE vbap-werks,
        vkgrp              TYPE char40,
        vkbur              TYPE vbak-vkbur,
        kunnr              TYPE vbak-kunnr,
        name1              TYPE kna1-name1,
        auart              TYPE vbak-auart,
        zterm              TYPE vbkd-zterm,
        text1              TYPE t052u-text1,
        vbeln_s            TYPE vbak-vbeln,
        vbeln_p            TYPE vbak-vbeln,
        vbeln              TYPE vbak-vbeln,
        vbeln_g            TYPE vbak-vbeln,
        erdat              TYPE vbak-erdat,
        waerk              TYPE vbak-waerk,
        totalq_ov          TYPE zfit0026-mont_moeda,
        totvl_ov           TYPE zfit0026-mont_moeda,
        netwr_l            TYPE vbap-netwr,
        mwsbp              TYPE vbap-mwsbp,
        data_venc          TYPE zfit0026-data_venc,
        data_venc_2        TYPE zfit0026-data_venc,
        forma_pag          TYPE zfit0026-forma_pag,
        taxa               TYPE zfit0026-taxa,
        mont_moeda         TYPE zfit0026-mont_moeda,
        mont_mi            TYPE zfit0026-mont_mi,
        docnum             TYPE zfit0026-docnum,
        moeda_forte        TYPE zfit0026-mont_moeda,
        moeda_inter        TYPE zfit0026-mont_mi,
        augbl              TYPE bsad-augbl,
        budat              TYPE bsad-budat,
        dmbe2              TYPE bsad-dmbe2,
        dmbtr              TYPE bsad-dmbtr,
        salus              TYPE bsad-dmbe2,
        salre              TYPE bsad-dmbtr,
        rfmng              TYPE rfmng,
        observacao         TYPE zfit0026-observacao,
        safra              TYPE zsdt0040-safra,
        id_order_ecommerce TYPE zsdt0040-id_order_ecommerce,
        cultura            TYPE zsdt0038-descricao,
        line_color(4)      TYPE c,
*        color_cell         TYPE lvc_t_scol,
        pgto_ant           TYPE char30,
        tx_multa           TYPE zsdt0051-tx_multa,
        tx_juros           TYPE zsdt0051-tx_juros,
        fkimg              TYPE vbrp-fkimg,
        ptax               TYPE zfit0026-taxa,
        vlr_multa_calc     TYPE zfit0026-vlr_multa_calc,
        vlr_multa_rbdo     TYPE zfit0026-vlr_multa_calc,
        vlr_juros_calc     TYPE zfit0026-vlr_juros_calc,
        vlr_juros_rbdo     TYPE zfit0026-vlr_juros_rbdo,
        vlr_desc_mult      TYPE zfit0026-vlr_desc_mult,
        vlr_desc_jros      TYPE zfit0026-vlr_desc_jros,
        referencia_nfe     TYPE j_1bnfdoc-nfenum,
        vlr_sald_fin_brl   TYPE zfit0026-mont_rbdo,
        vlr_sald_fin       TYPE zfit0026-mont_rbdo,
        data_pgto          TYPE zfit0026-data_pgto,
        mont_rbdo          TYPE zfit0026-mont_rbdo,
        vlr_sal_fat        TYPE zfit0026-mont_rbdo,
        vlr_referencia     TYPE vbrp-netwr,
        vlr_tot_ref        TYPE vbrp-netwr,
        vlr_total_ov       TYPE zfit0026-mont_moeda,
        sald_referencia    TYPE vbrp-netwr,
        ind_rec_total      TYPE char1,
        ind_rec_parc       TYPE char1,
        num_comp_adiant    TYPE zfit0026-num_comp_adiant,
        tprel              TYPE char2,
        tplin              TYPE char1,
      END OF ty_saida.

    TYPES:
     tyt_saida TYPE TABLE OF ty_saida .

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

    DATA git_data_inbound TYPE zfie0016 .
    DATA gv_area TYPE kokrs .
    DATA gv_perio TYPE monat .
    DATA gv_ano TYPE gjahr .
    DATA gv_protocolo TYPE string .
    DATA gv_prot TYPE string .
    DATA git_empresa TYPE zttsys_bukrs .
    DATA: git_cliente TYPE zttsys_kunnr,
          gv_venc_ini TYPE datum,
          gv_venc_fim TYPE datum.
    DATA it_conf TYPE tyt_conf .
    DATA:
      it_saida TYPE STANDARD TABLE OF ty_saida .
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
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_INT_IB_SD_REL_CONTAS_REC IMPLEMENTATION.


  METHOD constructor.

    me->zif_integracao_inject~at_id_interface    = me->at_id_interface.
    me->zif_integracao_inject~at_tp_integracao   = zif_integracao=>at_tp_integracao_inbound.
    me->zif_integracao_inject~at_tp_canal        = zif_integracao=>at_tp_canal_comunica_http.
    me->zif_integracao_inject~at_tp_sincronia    = zif_integracao=>at_tp_sincronia_sincrona.
    me->zif_integracao_inject~at_autentica_opus  = zif_integracao=>at_id_interface_aut_opus_nao.
    me->zif_integracao_inject~at_send_autenticao = zif_integracao=>at_id_interface_aut_send_nao.

  ENDMETHOD.


  METHOD create_job.
*
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
**    LOOP AT git_ordens ASSIGNING FIELD-SYMBOL(<fs_ord>).
**      IF sy-tabix EQ 1.
**        ls_aufnr-low = <fs_ord>.
**      ENDIF.
**      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_ord> high = '' ) TO lt_aufnr[].
**    ENDLOOP.
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
**      EXIT.
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
*        " Verificar se a variável já existe na tabela
*        SELECT *
*        FROM tvarvc
*        INTO TABLE @DATA(dummy)
*        WHERE name = @ls_tvarvc-name.
*
*        IF sy-subrc = 0.
*          " Atualizar o registro existente
*          UPDATE tvarvc SET  low = lv_jobcount
*                      WHERE  name = iv_job.
*          ev_ret = abap_true.
*        ELSE.
*          " Inserir novo registro
*          INSERT INTO tvarvc VALUES ls_tvarvc.
*          ev_ret = abap_true.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD get_data.

*    DATA nmjob TYPE tbtco-jobname.
*
*    nmjob = gv_protocolo.
*    FREE: it_saida.
*    SELECT
*       substring( a~jobname,1,4 ) AS empresa "area
*       ,a~jobname
*       ,a~jobcount
*       ,CASE WHEN a~reluname <> ' ' THEN a~reluname ELSE a~sdluname END AS executor_nm
*       ,a~reldate AS dtstart
*       ,a~reltime AS hrstart
*       ,a~enddate AS dtfinish
*       ,a~endtime AS hrfinish
*       ,CASE
*       WHEN a~status = 'A' THEN '@0A@'
*       WHEN a~status = 'F' THEN '@08@'
*       ELSE '@09@'
*       END AS job_status
*       ,CASE
*       WHEN a~status = 'A' THEN 'Cancelado'
*       WHEN a~status = 'F' THEN 'Completado'
*       WHEN a~status = 'P' THEN 'Agendado'
*       WHEN a~status = 'R' THEN 'Ativo'
*       WHEN a~status = 'S' THEN 'Lançado'
*       END AS job_desc
*       ,' ' AS liq_status
*       ,CAST( b~listident AS INT4 ) AS ident
*       FROM tbtco AS a
*       LEFT JOIN tbtcp AS b ON a~jobname = b~jobname
*       AND a~jobcount = b~jobcount
**   where substring( a~jobname,1,8 ) = nmjob
*   WHERE a~jobname = @nmjob
*   AND a~reldate > 0
*   INTO TABLE @it_saida.
*
*    SORT it_saida BY jobname ASCENDING dtstart DESCENDING hrstart DESCENDING.
*
*    DELETE ADJACENT DUPLICATES FROM it_saida COMPARING jobname.
*
*    DATA ident TYPE i.
*    DATA lt_spool TYPE STANDARD TABLE OF lvc_s_1022 INITIAL SIZE 0 .
*    DATA l_file_length  TYPE rspoid.
*    DATA spool_contents TYPE soli_tab.
*    DATA spool_contstr  TYPE string.
*
*    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_get_spoll>).
*      CLEAR: l_file_length,ident.
*      FREE: lt_spool.
*
*      IF <_get_spoll>-ident > 0.
*
*        "Verificar se a ordem spool ainda esta disponivel.
*        CALL FUNCTION 'RSPO_CHECK_JOB_ID_PERMISSION'
*          EXPORTING
*            rqident       = <_get_spoll>-ident
*            access        = 'DISP'
*          EXCEPTIONS
*            no_such_job   = 1
*            no_permission = 2
*            OTHERS        = 3.
*        IF sy-subrc EQ 0.
*
*          CALL FUNCTION 'RSPO_RETURN_ABAP_SPOOLJOB'
*            EXPORTING
*              rqident = <_get_spoll>-ident
*            TABLES
*              buffer  = spool_contents.
*          IF sy-subrc = 0.
*
*            DATA: it_char255 TYPE TABLE OF char255,
*                  wa_char255 TYPE char255.
*            DATA: it_string TYPE TABLE OF string,
*                  wa_string TYPE string.
*
*
*            LOOP AT spool_contents INTO DATA(wa_cont).
*              IF wa_cont CS '----------------------------------------------------------------------------'
*               OR wa_cont CS 'Programa Mapa de Comprovação de Investimentos'.
*                CONTINUE.
*              ENDIF.
*              wa_char255 = wa_cont.
*              APPEND wa_char255 TO it_char255.
*              CLEAR wa_char255.
*            ENDLOOP.
*
*            DATA: lv_index TYPE sy-tabix.
*
*            LOOP AT it_char255 INTO wa_char255.
*              me->gv_string_job = me->gv_string_job && wa_char255.
*            ENDLOOP.
*
**            LOOP AT spool_contents INTO DATA(lr_spool).
**              IF sy-tabix = 22 .
**                <_get_spoll>-liq_result = lr_spool.
**                CONDENSE lr_spool NO-GAPS.
**
**                CLEAR: <_get_spoll>-liq_status.
**                CASE lr_spool.
**                  WHEN 'Processamentoencerradocomerros'.
**                    <_get_spoll>-liq_status = '@0A@'.
**                  WHEN OTHERS.
**                    <_get_spoll>-liq_status = '@08@'.
**                ENDCASE.
**              ENDIF.
**            ENDLOOP.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*
*    IF spool_contents[] IS INITIAL.
*
*      READ TABLE it_saida INTO DATA(wa_saida) INDEX 1.
*      IF sy-subrc IS INITIAL.
*        IF wa_saida-job_desc NE 'Completado'.
*          IF wa_saida-job_desc EQ 'Cancelado'.
*            me->gv_string_job = '"Erro no processamento!"'.
*          ELSE.
*            me->gv_string_job = '"Informações ainda não disponíveis!"'.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE * FROM zfit0020
*            INTO @DATA(wa_fit0020)
*            WHERE jobname EQ @nmjob.
*          IF sy-subrc IS INITIAL AND wa_fit0020-info IS NOT INITIAL.
*            me->gv_string_job = wa_fit0020-info.
*          ELSE.
*            me->gv_string_job = '"Não foram encontrados dados para os parametros informados!"'.
*          ENDIF.
*        ENDIF.
*      ELSE.
*        me->gv_string_job = '"Protocolo não encontrado!"'.
*      ENDIF.
*    ELSE.
*      FREE:spool_contents.
*    ENDIF.
*
**    SORT it_saida BY liq_status.
**    DELETE it_saida WHERE liq_status EQ space.

  ENDMETHOD.


  METHOD get_filial_matriz.
*
*    DATA: wl_branch_detail TYPE bapibranch.
*
*    CLEAR: wl_branch_detail, es_branch.
*
*    CHECK iv_bukrs IS NOT INITIAL.
*
*    SELECT SINGLE *
*      FROM j_1bbranch INTO @DATA(_wl_branch)
*     WHERE bukrs  = @iv_bukrs
*       AND branch = '0001'. "Matriz
*
*    IF sy-subrc = 0.
*      CALL FUNCTION 'BAPI_BRANCH_GETDETAIL'
*        EXPORTING
*          company       = _wl_branch-bukrs
*          branch        = _wl_branch-branch
*        IMPORTING
*          branch_detail = wl_branch_detail.
*
*      es_branch-cnpj = wl_branch_detail-cgc_number.
*      es_branch-name = wl_branch_detail-name.
*
*      IF wl_branch_detail-adrnr IS NOT INITIAL.
*        SELECT SINGLE *
*          FROM adrc INTO @DATA(_wl_adrc)
*         WHERE addrnumber = @wl_branch_detail-adrnr.
*
*        IF ( sy-subrc = 0 ).
*          es_branch-country =  _wl_adrc-country.
*          es_branch-region  =  _wl_adrc-region.
*          es_branch-street  =  _wl_adrc-street.
*          es_branch-city2   =  _wl_adrc-city2.
*          es_branch-city1   =  _wl_adrc-city1.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD recovered_job.
*
*    DATA: wa_saida TYPE zpme0087.
*    CLEAR: wa_saida.
*
*    READ TABLE it_saida INTO wa_saida INDEX 1.
*    IF wa_saida-ident IS NOT INITIAL.
*
*      DATA: lv_spoolid    TYPE tsp01-rqident,
*            lv_pdf_size   TYPE i,               " PDF size
*            lt_pdf_table  TYPE TABLE OF tline,  " Internal table to store PDF
*            lv_html       TYPE string,
*            lv_base64     TYPE string,
*            lv_xstring    TYPE xstring,
*            lt_binary     TYPE TABLE OF x255,   " Binary internal table
*            lt_lines      TYPE i,
*            ev_spoolid    TYPE tsp01-rqident,
*            lv_bin_length TYPE i.               " Binary length
*
*      CLEAR:lv_spoolid,lv_base64,lv_pdf_size,lt_pdf_table[],lv_xstring,lt_binary,lv_bin_length.
*
*      lv_spoolid = wa_saida-ident.
*
*      CALL FUNCTION 'CONVERT_ABAPSPOOLJOB_2_PDF'
*        EXPORTING
*          src_spoolid              = lv_spoolid   " Spool Request ID
*          no_dialog                = 'X'
*          dst_device               = 'PDF1'
*          pdf_destination          = 'X'
*        IMPORTING
*          pdf_bytecount            = lv_pdf_size  " PDF Size
*          pdf_spoolid              = ev_spoolid
*          bin_file                 = lv_xstring
*        TABLES
*          pdf                      = lt_pdf_table " PDF content
*        EXCEPTIONS
*          err_no_abap_spooljob     = 1
*          err_no_spooljob          = 2
*          err_no_permission        = 3
*          err_conv_not_possible    = 4
*          err_bad_destdevice       = 5
*          user_cancelled           = 6
*          err_spoolerror           = 7
*          err_temseerror           = 8
*          err_btcjob_open_failed   = 9
*          err_btcjob_submit_failed = 10
*          err_btcjob_close_failed  = 11
*          OTHERS                   = 12.
*
*      CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
*        EXPORTING
*          input  = lv_xstring
*        IMPORTING
*          output = lv_base64.
*
*      IF lv_base64 IS NOT INITIAL.
*        gv_string_job = lv_base64.
*      ENDIF.
*
**      lv_html = '<html><body><embed src="data:application/pdf;base64,' && lv_base64_pdf && '" width="100%" height="100%"></embed></body></html>'.
**        cl_abap_browser=>show_html( html_string = lv_html title = 'PDF' ).
*    ENDIF.
  ENDMETHOD.


  METHOD run.
    TYPES: ty_rsparamsl_255_t TYPE STANDARD TABLE OF rsparamsl_255 WITH NON-UNIQUE DEFAULT KEY.

    TYPES: BEGIN OF ty_range,
             sign   TYPE char1,
             option TYPE char2,
             low    TYPE char10,
             high   TYPE char10,
           END OF ty_range.

    FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                   <lt_data_line> TYPE ANY TABLE,
                   <ls_data>      TYPE any,
                   <ls_data_line> TYPE any.

    DATA: lr_data            TYPE REF TO data,
          lr_data_line       TYPE REF TO data,
          w_saida            TYPE ty_saida,
          lr_data_descr      TYPE REF TO cl_abap_datadescr,
          lr_data_line_descr TYPE REF TO cl_abap_datadescr.
    DATA: lr_bukrs TYPE RANGE OF ty_range,
          lr_kunnr TYPE RANGE OF ty_range,
          lr_venc  TYPE RANGE OF ty_range.
    DATA: w_bukrs LIKE LINE OF lr_bukrs,
          w_kunnr LIKE LINE OF lr_bukrs,
          w_venc  LIKE LINE OF lr_bukrs.

    DATA: it_saida_aux TYPE TABLE OF ty_saida, "// wbarbosa 12/12/2024 US-140385
          ls_saida_aux TYPE ty_saida. "// wbarbosa 12/12/2024 US-140385

    LOOP AT git_empresa ASSIGNING FIELD-SYMBOL(<fs_empresa>).
      w_bukrs-sign = 'I'.
      w_bukrs-option = 'EQ'.
      w_bukrs-low = <fs_empresa>.
      APPEND w_bukrs TO lr_bukrs.
    ENDLOOP.

    LOOP AT git_cliente ASSIGNING FIELD-SYMBOL(<fs_cliente>).
      w_kunnr-sign = 'I'.
      w_kunnr-option = 'EQ'.
      w_kunnr-low = <fs_cliente>.
      APPEND w_kunnr TO lr_kunnr.
    ENDLOOP.

    w_venc-sign = 'I'.
    w_venc-option = 'EQ'.
    w_venc-low = gv_venc_ini.
    w_venc-high = gv_venc_fim.
    APPEND w_venc TO lr_venc.

    IF <lt_data> IS ASSIGNED.
      CLEAR: <lt_data>[].
    ENDIF.

    IF <lt_data_line> IS ASSIGNED.
      CLEAR: <lt_data_line>[].
    ENDIF.

    IF <ls_data> IS ASSIGNED .
      CLEAR: <ls_data>.
    ENDIF.

    IF <ls_data_line> IS ASSIGNED .
      CLEAR: <ls_data_line>.
    ENDIF.

    FREE: lr_data, lr_data_line, lr_data_descr, lr_data_line_descr.

    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).


    SUBMIT zsdr0020    WITH p_bukrs IN lr_bukrs
                       WITH p_kunnr IN lr_kunnr
                       WITH p_venc  IN lr_venc
                       WITH p_varia EQ '/IPIRANGA'
                       EXPORTING LIST TO MEMORY
                       AND RETURN.

    TRY.
        cl_salv_bs_runtime_info=>get_data_ref(
          IMPORTING
            r_data_descr      = lr_data_descr
            r_data_line_descr = lr_data_line_descr ).

        CHECK ( lr_data_descr IS NOT INITIAL ) OR ( lr_data_line_descr IS NOT INITIAL ).

        CREATE DATA lr_data      TYPE HANDLE lr_data_descr.
        CREATE DATA lr_data_line TYPE HANDLE lr_data_line_descr.

        ASSIGN lr_data->*      TO <lt_data>.
        ASSIGN lr_data_line->* TO <lt_data_line>.

        cl_salv_bs_runtime_info=>get_data( IMPORTING t_data      = <lt_data>
                                                     t_data_line = <lt_data_line> ).

      CATCH cx_salv_bs_sc_runtime_info.
    ENDTRY.

    cl_salv_bs_runtime_info=>clear_all( ).

    ASSIGN lr_data->*      TO <ls_data>.
    ASSIGN lr_data_line->* TO <ls_data_line>.

    IF <lt_data> IS ASSIGNED.
      LOOP AT <lt_data> ASSIGNING <ls_data>.
        MOVE-CORRESPONDING <ls_data> TO w_saida.
        APPEND w_saida TO it_saida_aux.
      ENDLOOP.
    ENDIF.

    DELETE it_saida_aux WHERE zid_lanc IS NOT INITIAL.

    CLEAR w_saida.
    LOOP AT it_saida_aux INTO ls_saida_aux.

      w_saida-vbeln_s   = ls_saida_aux-vbeln_s.
      w_saida-safra     = ls_saida_aux-safra.
      w_saida-totalq_ov = ls_saida_aux-totalq_ov.
      w_saida-totvl_ov  = ls_saida_aux-totvl_ov.
      w_saida-netwr_l   = ls_saida_aux-netwr_l.
      w_saida-rfmng     = ls_saida_aux-rfmng.
      w_saida-waerk     = ls_saida_aux-waerk.
      w_saida-cultura   = ls_saida_aux-cultura.

      COLLECT w_saida INTO it_saida.

    ENDLOOP.

  ENDMETHOD.


  METHOD set.

*GV_AREA         Instance Attribute Public  Type  KOKRS
*GV_PERIO        Instance Attribute Public  Type  MONAT
*GV_ANO          Instance Attribute Public  Type  GJAHR
*GV_PROTOCOLO    Instance Attribute Public  Type  STRING
*GIT_ORDENS      Instance Attribute Public  Type  ZTTSYS_AUFNRL

*    gv_area       = i_area.
*    gv_perio      = i_perio.
*    gv_ano        = i_ano.
*    gv_protocolo  = i_prot.
**    git_ordens[]  = it_ordens[].

  ENDMETHOD.


  METHOD zif_integracao_inbound~configure_server.

    DATA: lva_reason TYPE string,
          lva_code   TYPE char3.

    i_http_server->response->set_header_field( name = 'Content-Type'  value = 'application/json; charset=UTF-8' ).

    IF me->zif_integracao_inbound~at_zintegracao_log-nm_code IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = me->zif_integracao_inbound~at_zintegracao_log-nm_code
        IMPORTING
          output = lva_code.

      CALL FUNCTION 'ZHTTP_RET_DS_STATUS_RESPONSE'
        EXPORTING
          i_code        = lva_code
        IMPORTING
          e_desc_status = lva_reason.

      i_http_server->response->set_status(
        EXPORTING
          code   = CONV #( lva_code )
          reason = CONV #( lva_reason )
      ).

    ENDIF.

  ENDMETHOD.


  METHOD zif_integracao_inbound~processar_requisicao.

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

  ENDMETHOD.


  METHOD zif_integracao_inbound~set_data.

    r_if_integracao_inbound = me.
    me->zif_integracao_inject~at_info_request_http = i_info.

  ENDMETHOD.


  METHOD zif_integracao_inbound~validar_dados_inbound.

    DATA:
      lit_data_inbound TYPE zsde0021,
      wa_bukrs         TYPE bukrs,
      wa_kunnr         TYPE kunnr,
      ox_err           TYPE REF TO cx_root,
      lv_message       TYPE string,
      wa_ordens        TYPE zsys_aufnr.

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
          IF lit_data_inbound-empresa[] IS NOT INITIAL.
            LOOP AT lit_data_inbound-empresa[] ASSIGNING FIELD-SYMBOL(<fs_empresa>).
              wa_bukrs = <fs_empresa>.

              APPEND wa_bukrs TO git_empresa.
              FREE wa_bukrs.
            ENDLOOP.
          ELSE.
            me->gv_string_job = '"Empresa(s) informação obrigatória!"'.
            EXIT.
          ENDIF.

          IF lit_data_inbound-cliente[] IS NOT INITIAL.
            LOOP AT lit_data_inbound-cliente[] ASSIGNING FIELD-SYMBOL(<fs_cliente>).
              wa_kunnr = <fs_cliente>.

              APPEND wa_kunnr TO git_cliente.
              FREE wa_kunnr.
            ENDLOOP.
          ENDIF.

          IF lit_data_inbound-dt_venc_ini IS NOT INITIAL.

            gv_venc_ini = lit_data_inbound-dt_venc_ini+6(4) && lit_data_inbound-dt_venc_ini+3(2) && lit_data_inbound-dt_venc_ini(2). " 12/01/2024

          ELSE.
            gv_venc_ini = sy-datum.
          ENDIF.

          IF lit_data_inbound-dt_venc_fim IS NOT INITIAL.

            gv_venc_fim = lit_data_inbound-dt_venc_fim+6(4) && lit_data_inbound-dt_venc_fim+3(2) && lit_data_inbound-dt_venc_fim(2).

          ELSE.
            gv_venc_fim = sy-datum.
          ENDIF.

          IF gv_venc_ini IS NOT INITIAL AND gv_venc_fim IS NOT INITIAL.
            DATA lv_ano(4) TYPE n.
            lv_ano = gv_venc_fim(4) - gv_venc_ini(4).
            IF lv_ano GT 4.
              me->gv_string_job = '"Datas limite de 4 anos!"'.
              EXIT.
            ENDIF.
          ELSEIF gv_venc_ini IS INITIAL AND gv_venc_fim IS INITIAL.

            me->gv_string_job = '"Data informação obrigatória!"'.
            EXIT.
          ELSE.
            IF gv_venc_ini IS INITIAL AND gv_venc_fim IS NOT INITIAL.
              gv_venc_ini = gv_venc_fim.
            ENDIF.
          ENDIF.

          me->run( IMPORTING lt_message = lv_message
                             it_conf    = it_conf
                            ).
        ENDIF.

      CATCH cx_root INTO ox_err.
        r_msg_erro = |{ ox_err->get_text( ) }| & |{ 'Erro ZCL_INT_IB_SD_REL_CONTAS_REC!'(e02) }|.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_integracao_inject~get_header_request_http.

    r_if_integracao_inject = me.
    e_header_fields = me->zif_integracao_inject~at_header_fields.

  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_error_outbound_msg.
    e_sucesso = abap_false.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_before_send_outbound_msg.


  ENDMETHOD.


  METHOD zif_integracao_inject~set_header_request_http.
    r_if_integracao_inject = me.
    me->zif_integracao_inject~at_header_fields = i_header_fields.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_integrar_inbound.

    DATA: lit_data_inbound  TYPE tyt_saida,
          lit_data_inbounds TYPE string,
          lva_operacao      TYPE string,
          lva_ds_erro       TYPE string.

    r_if_integracao_inject = me.
    CLEAR: e_msg_erro, e_sucesso, e_nm_code, e_msg_outbound.

    e_msg_erro = me->zif_integracao_inbound~validar_dados_inbound( i_data_inbound = i_msg_inbound  ).

    IF it_saida[] IS NOT INITIAL.

      IF i_msg_inbound IS NOT INITIAL.
        /ui2/cl_json=>deserialize( EXPORTING json = i_msg_inbound CHANGING data = lit_data_inbound ).
      ENDIF.
      DATA lv_json TYPE string.
      lv_json = /ui2/cl_json=>serialize( data = it_saida compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-camel_case ).

*      me->gv_string_job = i_msg_inbound.
      me->gv_string_job = lv_json.

    ELSE.
      IF me->gv_string_job IS INITIAL.
        me->gv_string_job = '"Dados não encontrados!"'.
      ENDIF.
    ENDIF.

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

    CASE me->zif_integracao_inject~at_info_request_http-ds_metodo.
      WHEN 'POST'. "Inclusão/Modificação

        IF gv_string_job IS INITIAL.
          e_msg_erro = 'Houve um erro ao gravar o registro no banco de dados!'.
        ENDIF.

      WHEN OTHERS.
        e_msg_erro = 'Operação não prevista!'.
    ENDCASE.

    IF e_msg_erro IS NOT INITIAL.
      EXIT.
    ENDIF.

    IF e_msg_erro IS INITIAL.

      e_sucesso   = abap_true.
      e_nm_code   = '200'.
      e_msg_erro  = 'Ok'.

      IF me->gv_string_job IS NOT INITIAL.
        e_msg_outbound = ' {' && cl_abap_char_utilities=>newline &&
                         '   "Status_code" : "' && e_nm_code          &&  '" ,'  && cl_abap_char_utilities=>newline &&
                         '   "Resultado" : ' && | { me->gv_string_job } |  &&  ''  && cl_abap_char_utilities=>newline &&
                         ' }'.
      ENDIF.
    ELSE.

      IF gv_string_job IS NOT INITIAL.
        e_sucesso   = abap_true.
        e_nm_code   = '200'.
        e_msg_erro  = 'Ok'.
        e_msg_outbound = ' {' && cl_abap_char_utilities=>newline &&
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


  METHOD zif_integracao_inject~set_integrar_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_parametro.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_inbound.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.


  METHOD zif_integracao_inject~set_processa_retorno.
    r_if_integracao_inject = me.
    e_sucesso = abap_true.
  ENDMETHOD.
ENDCLASS.
