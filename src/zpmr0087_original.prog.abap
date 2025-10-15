*&---------------------------------------------------------------------*
*& Report zpmr0087
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpmr0087_original.


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*
*SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*  SELECT-OPTIONS:  s_bukrs  FOR coep-bukrs OBLIGATORY.
*  PARAMETERS: p_kokrs TYPE coep-kokrs DEFAULT 'MAGI' OBLIGATORY,
*              p_perio TYPE coep-perio OBLIGATORY,
*              p_gjahr TYPE coep-gjahr OBLIGATORY.
*
*SELECTION-SCREEN: END OF BLOCK b1.

*----------------------------------------------------------------------*
* INCLUDE                                                              *
*----------------------------------------------------------------------*
INCLUDE ZPMR0087_TOP_ORIGINAL.
*INCLUDE zpmr0087_top.
INCLUDE ZPMR0087_CLA_ORIGINAL.
*INCLUDE zpmr0087_cla.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.


  p_perio = sy-datum+4(2).
  p_gjahr = sy-datum(4).

  DATA: lr_bukrs TYPE RANGE OF bukrs.

  TYPES: BEGIN OF ty_param,
           low TYPE tvarvc-low,
         END OF ty_param.

  DATA: lt_param TYPE TABLE OF ty_param WITH HEADER LINE.


  SELECT * FROM tvarvc WHERE name = 'ZPM0106' INTO TABLE @DATA(lt_tvarv).
  IF sy-subrc = 0.
    LOOP AT lt_tvarv ASSIGNING FIELD-SYMBOL(<fs_tvarv>).
      SPLIT <fs_tvarv>-low AT ';' INTO TABLE lt_param.
      APPEND lt_param.
    ENDLOOP.

    lr_bukrs = VALUE #( FOR ls_value IN lt_param ( sign = 'I'
                                                 option = 'EQ'
                                                 low = ls_value-low ) ).
  ENDIF.

  CLEAR gt_job.

  SELECT * FROM tvarvc
   INTO TABLE gt_job
   WHERE name LIKE 'RKO7KO8G%'.

  IF sy-subrc <> 0.
    CLEAR: gt_job, gt_job[].
  ENDIF.


  LOOP AT lr_bukrs INTO DATA(wa_bukrs).
    CHECK wa_bukrs-low IS NOT INITIAL.

    APPEND INITIAL LINE TO s_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    <fs_bukrs>-sign = 'I'.
    <fs_bukrs>-option = 'EQ'.

    APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    <fs_bukrs>-low = wa_bukrs-low.
    <fs_data>-bukrs = wa_bukrs-low.
    <fs_data>-perio = p_perio.
    <fs_data>-gjahr = p_gjahr.

    DATA(lv_jobname) = |{ gc_prog_exec && wa_bukrs-low }| .

    READ TABLE gt_job WITH KEY name = lv_jobname INTO DATA(wa).

    IF sy-subrc <> 0.
      <fs_data>-icon = gc_light_red.
    ELSE.

      PERFORM f_status_job USING wa-low
                                 lv_jobname
                        CHANGING gv_status.

      <fs_data>-desc = gv_status.

      CASE gv_status.

        WHEN 'Finalizado'.
          <fs_data>-icon = gc_light_green.

        WHEN 'Abortado' OR ' '.
          <fs_data>-icon = gc_light_red.

        WHEN OTHERS.
          <fs_data>-icon = gc_light_yellow.

      ENDCASE.

    ENDIF.

  ENDLOOP.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

END-OF-SELECTION.

  IF sy-batch IS INITIAL.
    PERFORM display_data  CHANGING gt_data
                                   gr_alv.

  ELSE.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<data>).

      PERFORM selecionar_dados USING <data>
                                         ''.
      PERFORM criar_job USING <data>.

    ENDLOOP.
  ENDIF.

FORM selecionar_dados USING p_data TYPE lin_data
                            p_anterior TYPE c.

  DATA: c_30(2)   TYPE c VALUE 30,
        c_koao(4) TYPE c VALUE 'KOAO'.

  DATA(lv_perio) = p_perio.

  IF p_anterior IS NOT INITIAL. "Clique no botão periodo anterior

    DATA(lv_mes) = sy-datum+4(2).

    IF lv_mes = '01'.

      lv_perio = '012'.
      p_gjahr = p_gjahr - 1.

    ELSE.
      lv_perio = lv_mes - 1.
    ENDIF.
  ENDIF.

  SELECT * FROM coep
    INTO TABLE @gt_coep
  WHERE bukrs = @p_data-bukrs
    AND kokrs = @p_kokrs
    AND perio = @lv_perio
    AND gjahr = @p_gjahr
    AND autyp = @c_30.

  IF sy-subrc = 0.
    DELETE gt_coep WHERE vrgng = c_koao.
  ENDIF.

  IF gt_coep[] IS INITIAL.
    MESSAGE 'Ordens a liquidar não encontradas para este Período/Empresa' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM display_data CHANGING p_data TYPE tab_data
                            p_alv  TYPE REF TO  cl_salv_table.


  CHECK gt_data[] IS NOT INITIAL.


  DATA:
    lv_text    TYPE        string,
    lv_tooltip TYPE        string.

  DATA:
    lr_layout   TYPE REF TO cl_salv_layout.

  DATA:
    ls_key      TYPE        salv_s_layout_key.


  TRY.
      cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>default_screen
                              IMPORTING r_salv_table = p_alv
                              CHANGING  t_table      = p_data ).
    CATCH cx_root.
*   maybe some errorhandling here - just haven't made up my mind yet
  ENDTRY.

  TRY.
*      enable buttons for SALV
      p_alv->get_functions( )->set_all( abap_true ).


      CLEAR: lv_text, lv_tooltip.

      lv_text     = 'Atualizar Status Job'.
*      lv_tooltip  = ''.


      p_alv->get_functions( )->add_function( name     = |{ gc_btn_refresh_job }|
                                             icon     = |{ gc_icon_refresh  }|
                                             text     = lv_text
                                             tooltip  = lv_tooltip
                                             position = if_salv_c_function_position=>right_of_salv_functions ).


      lv_text     = 'Iniciar Job'.
      lv_tooltip  = 'Cria job - Período atual'.


      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job }|
                                             icon     = |{ gc_background_job }|
                                             text     = lv_text
                                             tooltip  = lv_tooltip
                                             position = if_salv_c_function_position=>right_of_salv_functions ).

      lv_text     = 'Iniciar Job ( período anterior)'.
      lv_tooltip  = 'Cria job - Período anterior'.


      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job_ant }|
                                             icon     = |{ gc_background_job_ant }|
                                             text     = lv_text
                                             tooltip  = lv_tooltip
                                             position = if_salv_c_function_position=>right_of_salv_functions ).


      CLEAR: lv_text, lv_tooltip.

      lv_text     = 'Cancelar Job'.
*     lv_tooltip  = 'Botão 2 dica'.

      p_alv->get_functions( )->add_function( name     = |{ gc_btn_close_job }|
                                             icon     = |{ gc_terminated_job }|
                                             text     = lv_text
                                             tooltip  = lv_tooltip
                                             position = if_salv_c_function_position=>right_of_salv_functions ).
**********************************************************************145948 - Ajuste para visualizar erros ZPM0106 - PSA
      CLEAR: lv_text, lv_tooltip.

      lv_text     = 'Erros'.
      lv_tooltip  = 'Erros de KO8G'.

      p_alv->get_functions( )->add_function( name     = |ERROS|
                                             icon     = |@UI@|
                                             text     = lv_text
                                             tooltip  = lv_tooltip
                                             position = if_salv_c_function_position=>right_of_salv_functions ).
**********************************************************************
*      set event handler
      SET HANDLER lcl_events=>on_toolbar_click FOR p_alv->get_event( ).

    CATCH cx_root.
*     maybe some errorhandling here - just haven't made up my mind yet
  ENDTRY.


  TRY.

      p_alv->get_columns( )->get_column( 'ICON' )->set_long_text( 'Stat.Job' ).
      p_alv->get_columns( )->get_column( 'ICON' )->set_medium_text( 'Status Job' ).
      p_alv->get_columns( )->get_column( 'ICON' )->set_short_text( 'Status Job' ).

      p_alv->get_columns( )->set_optimize( abap_true ).

    CATCH cx_root.
*     maybe some errorhandling here - just haven't made up my mind yet
  ENDTRY.

  TRY.

      p_alv->get_columns( )->get_column( 'DESC' )->set_long_text( 'Descrição' ).
      p_alv->get_columns( )->get_column( 'DESC' )->set_medium_text( 'Desc.' ).
      p_alv->get_columns( )->get_column( 'DESC' )->set_short_text( 'Desc.' ).

      p_alv->get_columns( )->set_optimize( abap_true ).

    CATCH cx_root.
*     maybe some errorhandling here - just haven't made up my mind yet
  ENDTRY.



  TRY.

      lr_layout = p_alv->get_layout( ).

*     set the Layout Key
      ls_key-report = sy-repid.
      lr_layout->set_key( ls_key ).

*     set usage of default Layouts
      lr_layout->set_default( abap_true ).

*      p_alv->get_selections( )->set_selection_mode( cl_salv_selections=>multiple ).
      p_alv->get_selections( )->set_selection_mode( cl_salv_selections=>row_column ).
    CATCH cx_root.
*     maybe some errorhandling here - just haven't made up my mind yet
  ENDTRY.

*  display the SALV
  p_alv->display( ).

  WRITE: space.


ENDFORM.


FORM open_job       USING p_rows  TYPE salv_t_row
                          p_anterior TYPE c
                    CHANGING p_data  TYPE tab_data.


  DATA:
    lv_subrc  TYPE sy-subrc.



  LOOP AT p_rows ASSIGNING FIELD-SYMBOL(<row>).

    READ TABLE p_data ASSIGNING FIELD-SYMBOL(<data>) INDEX <row>.

    IF sy-subrc = 0.

      PERFORM valida_job USING <data>-bukrs.
      PERFORM selecionar_dados USING <data>
                                     p_anterior.

      IF sy-sysid <> 'DEV'.
        CHECK gt_coep[] IS NOT INITIAL .
      ENDIF.

      PERFORM criar_job USING <data>.

      CLEAR:
        lv_subrc.

    ENDIF.
  ENDLOOP.

ENDFORM.

FORM close_job    USING p_rows  TYPE salv_t_row
                    CHANGING p_data  TYPE tab_data.


  DATA:
    lv_subrc  TYPE sy-subrc.



  LOOP AT p_rows ASSIGNING FIELD-SYMBOL(<row>).

    READ TABLE p_data ASSIGNING FIELD-SYMBOL(<data>) INDEX <row>.

    IF sy-subrc = 0.

      DATA(lv_jobname) = |{ gc_prog_exec && <data>-bukrs }|.
      READ TABLE gt_job INTO DATA(wa_job) WITH KEY name = lv_jobname.
      IF sy-subrc = 0.

        PERFORM cancela_job USING wa_job-name
                                  wa_job-low
                                  <data>-bukrs.

      ENDIF.

      CLEAR:
        lv_subrc.

    ENDIF.
  ENDLOOP.

ENDFORM.


FORM criar_job USING p_data TYPE lin_data.

  DATA: lv_jobname   TYPE btcjob,  " Nome do job
        lv_jobcount  TYPE tbtcjob-jobcount,               " Identificador do job
        lv_startdate TYPE d,
        lv_starttime TYPE t,
        lv_enddate   TYPE d,
        lv_endtime   TYPE t,
        ls_tvarvc    TYPE tvarvc
        .

  DATA: lv_ano            TYPE numc4,
        lv_mes            TYPE numc2,
        lv_ultimo_dia_mes TYPE endda,
        lv_qtd_dias       TYPE tbtcjob-prddays,
        lv_variant        TYPE raldb-variant.

*  " Definir a data e a hora de início (diariamente às 22h)
*  lv_startdate = sy-datum. " Data de hoje
*  lv_starttime = '220000'. " Hora (22:00:00)


*  " Extrai o ano e o mês da data de entrada
*  lv_ano = sy-datum(4).
*  lv_mes = sy-datum+4(2).
*
*  " Chama a função para obter o último dia do mês
*
*  CALL FUNCTION 'HR_HCP_GET_LAST_DAY_OF_MONTH'
*    EXPORTING
*      im_date              = sy-datum
*    IMPORTING
**     EX_DATE_IS_MONTHEND  =
*      ex_last_day_of_month = lv_ultimo_dia_mes.
*
*  lv_qtd_dias = ( lv_ultimo_dia_mes - sy-datum ) + 1. "Quantidade de dias até o final do mês, mais o dia atual.
*
*  IF lv_qtd_dias IS INITIAL.
*    lv_qtd_dias = 1.
*  ENDIF.

  PERFORM criar_variant USING gc_prog_variant.

  CONCATENATE gc_prog_exec p_data-bukrs INTO lv_jobname.
  CONCATENATE gc_liquida p_data-bukrs INTO lv_variant.

  " Criar um job
  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = lv_jobname
    IMPORTING
      jobcount         = lv_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao criar o job' TYPE 'E'.
    EXIT.
  ENDIF.

  DATA: testlauf  TYPE xfeld,
        c_magi(4) TYPE c VALUE 'MAGI'.

  SET PARAMETER ID 'CAC' FIELD c_magi.
  SET PARAMETER ID 'ORV' FIELD lv_variant.

  SUBMIT rko7ko8g
         USER sy-uname
         VIA JOB lv_jobname NUMBER lv_jobcount
*         WITH svald-value = 'MAGI'
*         WITH variant = lv_variant
         WITH perio = p_perio
         WITH gjahr = p_gjahr
         WITH testlauf = testlauf "test mode on/off
  AND RETURN.


*  " Adicionar um passo ao job
*  CALL FUNCTION 'JOB_SUBMIT'
*    EXPORTING
*      authcknam            = sy-uname
*      jobcount             = lv_jobcount
*      jobname              = lv_jobname
*      report               = gc_prog_exec
*      variant              = lv_variant
*    EXCEPTIONS
*      bad_priparams        = 1
*      jobname_missing      = 2
*      job_not_found        = 3
*      job_submit_failed    = 4
*      job_lock_failed      = 5
*      program_missing      = 6
*      program_start_failed = 7
*      OTHERS               = 8.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao adicionar passo ao job' TYPE 'E'.
    EXIT.
  ENDIF.

  " Fechar e executar imediatamente
  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = lv_jobcount
      jobname              = lv_jobname
      strtimmed            = abap_true
*     sdlstrtdt            = lv_startdate
*     sdlstrttm            = lv_starttime
*     prddays              = lv_qtd_dias  "Quantidade de dias que será executado
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
    EXIT.
  ELSE.

    IF sy-batch IS INITIAL.
      CONCATENATE gc_prog_exec p_data-bukrs INTO lv_jobname.

      ls_tvarvc-low  = lv_jobcount.
      ls_tvarvc-name = lv_jobname.

      " Verificar se a variável já existe na tabela
      SELECT *
      FROM tvarvc
      INTO TABLE @DATA(dummy)
      WHERE name = @ls_tvarvc-name.

      IF sy-subrc = 0.

        " Atualizar o registro existente
        UPDATE tvarvc SET  low = lv_jobcount
                    WHERE name = lv_jobname.

      ELSE.
        " Inserir novo registro
        INSERT INTO tvarvc VALUES ls_tvarvc.
      ENDIF.

      READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY bukrs = p_data-bukrs.
      IF sy-subrc = 0.
        <fs_data>-icon = gc_light_yellow.
        <fs_data>-desc = 'Em execução'.
      ENDIF.

      MESSAGE 'Job criado e será executado imediatamente' TYPE 'S'.
    ENDIF.
  ENDIF.

ENDFORM.

FORM criar_variant USING p_prog.

  DATA: lt_variant       TYPE disvariant,
        lt_vari_contents TYPE TABLE OF rsparams.

  DATA: ls_vari_desc TYPE varid,
        lt_vari_text TYPE TABLE OF varit.

  APPEND INITIAL LINE TO lt_vari_text ASSIGNING FIELD-SYMBOL(<fs_vari_text>).


  " Definir os valores da variante
  ls_vari_desc-mandt      = sy-mandt.
  ls_vari_desc-report     = p_prog.
  ls_vari_desc-environmnt = 'A'.
  <fs_vari_text>-mandt   = sy-mandt.
  <fs_vari_text>-langu   = sy-langu.
  <fs_vari_text>-report  = ls_vari_desc-report.
*  <fs_vari_text>-variant = ls_vari_desc-variant.
  <fs_vari_text>-vtext   = 'Variante gerada dinamicamente'.


  " Preencher a tabela de parâmetros
  LOOP AT gt_coep INTO DATA(wa_coep).

    CONCATENATE gc_liquida wa_coep-bukrs INTO ls_vari_desc-variant.
    <fs_vari_text>-variant = ls_vari_desc-variant.

    APPEND INITIAL LINE TO lt_vari_contents ASSIGNING FIELD-SYMBOL(<fs_vari_contents>).
    <fs_vari_contents>-selname  = 'AUFNR'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-aufnr.

    APPEND INITIAL LINE TO lt_vari_contents ASSIGNING <fs_vari_contents>.
    <fs_vari_contents>-selname  = 'BUKRS'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-bukrs.

*    APPEND INITIAL LINE TO lt_vari_contents ASSIGNING <fs_vari_contents>.
*    <fs_vari_contents>-selname  = 'PERIO'.
*    <fs_vari_contents>-kind     = 'S'.
*    <fs_vari_contents>-sign     = 'I'.
*    <fs_vari_contents>-option   = 'EQ'.
*    <fs_vari_contents>-low      = wa_coep-perio.

*    APPEND INITIAL LINE TO lt_vari_contents ASSIGNING <fs_vari_contents>.
*    <fs_vari_contents>-selname  = 'SVALD-VALUE'.
*    <fs_vari_contents>-kind     = 'S'.
*    <fs_vari_contents>-sign     = 'I'.
*    <fs_vari_contents>-option   = 'EQ'.
*    <fs_vari_contents>-low      = wa_coep-kokrs.

  ENDLOOP.

  " Criar a variante

  DATA: lv_curr_report  TYPE rsvar-report,
        lv_curr_variant TYPE rsvar-variant,
        rc              LIKE  sy-subrc.

  lv_curr_report = p_prog.

  CONCATENATE gc_liquida wa_coep-bukrs INTO lv_curr_variant.

*Check variant
  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report              = lv_curr_report
      variant             = lv_curr_variant
    IMPORTING
      r_c                 = rc
    EXCEPTIONS
      not_authorized      = 01
      no_report           = 02
      report_not_existent = 03
      report_not_supplied = 04.

  IF sy-subrc <> 0.
    MESSAGE e001(vl) WITH TEXT-e22 DISPLAY LIKE 'S'.
  ENDIF.

  IF rc = 0.                    " Variante existiert
    CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
      EXPORTING
        curr_report               = lv_curr_report
        curr_variant              = lv_curr_variant
        vari_desc                 = ls_vari_desc
      TABLES
        vari_contents             = lt_vari_contents
        vari_text                 = lt_vari_text
      EXCEPTIONS
        illegal_report_or_variant = 01
        illegal_variantname       = 02
        not_authorized            = 03
        not_executed              = 04
        report_not_existent       = 05
        report_not_supplied       = 06
        variant_doesnt_exist      = 07
        variant_locked            = 08
        selections_no_match       = 09.
  ELSE.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = lv_curr_report
        curr_variant              = lv_curr_variant
        vari_desc                 = ls_vari_desc
*       P_XML_TAB                 =
*       SUPPRESS_AUTHORITY_CHECK  =
      TABLES
        vari_contents             = lt_vari_contents
        vari_text                 = lt_vari_text
*       VSCREENS                  =
*       VARI_CONTENTS_L           =
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE 'Erro ao criar a variante' TYPE 'E'.
      EXIT.
    ENDIF.
  ENDIF.
ENDFORM.


FORM cancela_job USING p_jobname
                       p_jobcount
                       p_bukrs.


  DATA: iv_jobname  TYPE  btcjob,
        iv_jobcount TYPE  btcjobcnt.


  iv_jobname = p_jobname.
  iv_jobcount = p_jobcount.

  " Cancelar o job

  CALL FUNCTION 'SLTR_JOB_CANCEL'
    EXPORTING
      iv_jobname  = iv_jobname
      iv_jobcount = iv_jobcount
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao cancelar o job' TYPE 'E'.
  ELSE.

    DELETE FROM tvarvc WHERE name = p_jobname
                         AND low  = p_jobcount.

    READ TABLE gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WITH KEY bukrs = p_bukrs.
    IF sy-subrc = 0.
      <fs_data>-icon = gc_light_red.
    ENDIF.

    MESSAGE 'Job cancelado com sucesso' TYPE 'S'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form valida_job
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <DATA>
*&---------------------------------------------------------------------*
FORM valida_job  USING    p_bukrs.

  DATA: lv_jobname TYPE tvarvc-name.

*  CONCATENATE gc_prog p_bukrs INTO lv_jobname.
*
*  SELECT * FROM tvarvc
*   INTO TABLE gt_job
*   WHERE name = lv_jobname.
*
*  IF sy-subrc = 0. "Job já existe.
*    MESSAGE 'Já existe um job ativo para esta empresa' TYPE 'E'.
*    EXIT.
*  ENDIF.

ENDFORM.

FORM f_status_job USING p_jobcount
                        p_jobname
                  CHANGING p_status.

  DATA: l_jobname  TYPE tbtco-jobname,
        l_jobcount TYPE tbtco-jobcount.

  l_jobname = p_jobname .
  l_jobcount = p_jobcount.


  DATA:
    aborted     LIKE  tbtcv-abort,
    finished    LIKE  tbtcv-fin,
    preliminary LIKE  tbtcv-prelim,
    ready       LIKE  tbtcv-ready,
    running     LIKE  tbtcv-run,
    scheduled   LIKE  tbtcv-sched,
    suspended   TYPE  btcstatus,
    other       TYPE  btcstatus.

  CALL FUNCTION 'SHOW_JOBSTATE'
    EXPORTING
      jobcount         = l_jobcount
      jobname          = l_jobname
    IMPORTING
      aborted          = aborted
      finished         = finished
      preliminary      = preliminary
      ready            = ready
      running          = running
      scheduled        = scheduled
      suspended        = suspended
      other            = other
    EXCEPTIONS
      jobcount_missing = 1
      jobname_missing  = 2
      job_notex        = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

  IF aborted IS NOT INITIAL.
    p_status = 'Abortado'.

  ELSEIF finished IS NOT INITIAL.
    p_status = 'Finalizado'.

  ELSEIF preliminary IS NOT INITIAL.
    p_status = 'Preliminar'.

  ELSEIF ready IS NOT INITIAL.
    p_status = 'Pronto'.

  ELSEIF running IS NOT INITIAL.
    p_status = 'Em execução'.

  ELSEIF scheduled IS NOT INITIAL.
    p_status = 'Agendado'.

  ELSEIF suspended IS NOT INITIAL.
    p_status = 'suspenso'.

  ELSEIF other IS NOT INITIAL.
    p_status = 'Outros'.
  ENDIF.

ENDFORM.

FORM f_refresh.

  SELECT * FROM tvarvc WHERE name = 'ZPM0106' INTO TABLE @DATA(lt_tvarv).
  IF sy-subrc = 0.
    LOOP AT lt_tvarv ASSIGNING FIELD-SYMBOL(<fs_tvarv>).
      SPLIT <fs_tvarv>-low AT ';' INTO TABLE lt_param.
      APPEND lt_param.
    ENDLOOP.

    lr_bukrs = VALUE #( FOR ls_value IN lt_param ( sign = 'I'
                                                 option = 'EQ'
                                                 low = ls_value-low ) ).
  ENDIF.

  CLEAR gt_job.

  SELECT * FROM tvarvc
   INTO TABLE gt_job
   WHERE name LIKE 'RKO7KO8G%'.

  IF sy-subrc <> 0.
    CLEAR: gt_job, gt_job[].
  ENDIF.

  CLEAR: gt_data, gt_data[].

  LOOP AT lr_bukrs INTO DATA(wa_bukrs).
    CHECK wa_bukrs-low IS NOT INITIAL.

    APPEND INITIAL LINE TO s_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).
    <fs_bukrs>-sign = 'I'.
    <fs_bukrs>-option = 'EQ'.

    APPEND INITIAL LINE TO gt_data ASSIGNING FIELD-SYMBOL(<fs_data>).
    <fs_bukrs>-low = wa_bukrs-low.
    <fs_data>-bukrs = wa_bukrs-low.
    <fs_data>-perio = p_perio.
    <fs_data>-gjahr = p_gjahr.

    DATA(lv_jobname) = |{ gc_prog_exec && wa_bukrs-low }| .

    READ TABLE gt_job WITH KEY name = lv_jobname INTO DATA(wa).

    IF sy-subrc <> 0.
      <fs_data>-icon = gc_light_red.
    ELSE.

      PERFORM f_status_job USING wa-low
                                 lv_jobname
                        CHANGING gv_status.

      <fs_data>-desc = gv_status.

      CASE gv_status.

        WHEN 'Finalizado'.
          <fs_data>-icon = gc_light_green.

        WHEN 'Abortado' OR ' '.
          <fs_data>-icon = gc_light_red.

        WHEN OTHERS.
          <fs_data>-icon = gc_light_yellow.

      ENDCASE.

    ENDIF.

  ENDLOOP.

ENDFORM.
**********************************************************************145948 - Ajuste para visualizar erros ZPM0106 - PSA
FORM erros_job      USING p_rows  TYPE salv_t_row
                    CHANGING p_data  TYPE tab_data.


  DATA:
    lv_subrc  TYPE sy-subrc.
  BREAK-POINT.
  LOOP AT p_rows ASSIGNING FIELD-SYMBOL(<row>).

    READ TABLE p_data ASSIGNING FIELD-SYMBOL(<data>) INDEX <row>.

    IF sy-subrc = 0.


*        SUBMIT rko7ko8g
*                 USER sy-uname
*                 VIA JOB lv_jobname NUMBER lv_jobcount
*                 WITH svald-value = 'MAGI'
*                 WITH variant = lv_variant
*                 WITH perio = p_perio
*                 WITH gjahr = p_gjahr
*                 WITH testlauf = testlauf "test mode on/off
*                 EXPORTING LIST TO MEMORY AND RETURN
*               AND RETURN.

    ENDIF.
  ENDLOOP.

ENDFORM.
