*&---------------------------------------------------------------------*
*& Report zpmr0087
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zpmr0087.


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

include zpmr0087_top.
include zpmr0087_cla.

*----------------------------------------------------------------------*
* INITIALIZATION                                                       *
*----------------------------------------------------------------------*
initialization.

  p_perio = sy-datum+4(2).
  p_gjahr = sy-datum(4).

  data: lr_bukrs type range of bukrs.

  types: begin of ty_param,
           low type tvarvc-low,
         end of ty_param.

  data: lt_param type table of ty_param with header line.


  select * from tvarvc where name = 'ZPM0106' into table @data(lt_tvarv).
  if sy-subrc = 0.
    loop at lt_tvarv assigning field-symbol(<fs_tvarv>).
      split <fs_tvarv>-low at ';' into table lt_param.
      append lt_param.
    endloop.

    lr_bukrs = value #( for ls_value in lt_param ( sign = 'I'
                                                 option = 'EQ'
                                                 low = ls_value-low ) ).
  endif.

  clear gt_job.

  select * from tvarvc
   into table gt_job
   where name like 'RKO7KO8G%'.

  if sy-subrc <> 0.
    clear: gt_job, gt_job[].
  endif.


  loop at lr_bukrs into data(wa_bukrs).
    check wa_bukrs-low is not initial.

    append initial line to s_bukrs assigning field-symbol(<fs_bukrs>).
    <fs_bukrs>-sign = 'I'.
    <fs_bukrs>-option = 'EQ'.

    append initial line to gt_data assigning field-symbol(<fs_data>).
    <fs_bukrs>-low = wa_bukrs-low.
    <fs_data>-bukrs = wa_bukrs-low.
    <fs_data>-perio = p_perio.
    <fs_data>-gjahr = p_gjahr.

    clear:lv_jobname.
    lv_jobname = |{ gc_prog_exec && wa_bukrs-low }| .

    read table gt_job with key name = lv_jobname into data(wa).

    if sy-subrc <> 0.
      <fs_data>-icon = gc_light_red.
    else.

      perform f_status_job using wa-low
                                 lv_jobname
                        changing gv_status.

      <fs_data>-desc = gv_status.

      case gv_status.

        when 'Finalizado'.
          <fs_data>-icon = gc_light_green.

        when 'Abortado' or ' '.
          <fs_data>-icon = gc_light_red.

        when others.
          <fs_data>-icon = gc_light_yellow.

      endcase.

    endif.

  endloop.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT                                           *
*----------------------------------------------------------------------*
at selection-screen output.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN                                                  *
*----------------------------------------------------------------------*
at selection-screen.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
start-of-selection.

end-of-selection.

  "Seleciona stvarv area contabil.
*  select * from tvarvc into table @data(it_tvarvc)
*    where name = 'Z_ZPM0106_AREA_CONTABIL'.
*
*  "Seta area contabil.
*  p_kokrs = value #( for i in it_tvarvc ( sign = 'I' option = 'EQ' low = i-low ) ).


  if sy-batch is initial.
    "PERFORM display_data  CHANGING gt_data gr_alv.
    "CALL SCREEN 0100.
    set screen '100'.
*    CREATE OBJECT lo_report.
*    lo_report->get_data( ).
*    lo_report->generate_output( ).
  else.

    loop at gt_data assigning field-symbol(<data_get>).
      read table gt_data assigning field-symbol(<data>) with key bukrs = <data_get>-bukrs.
      if sy-subrc ne 0.
        continue.
      endif.

      perform selecionar_dados using <data> ''.
      if gt_coep[] is initial.
        continue.
      endif.

      perform criar_variant using gc_prog_variant.
      if lv_variant is initial and lv_jobname is initial.
        continue.
      endif.

      if gv_set_erro eq abap_false.
        perform criar_job using <data>.
      endif.
    endloop.
  endif.

form selecionar_dados using p_data type lin_data
                            p_anterior type c.

  types:
    begin of ty_coep_sum,
      aufnr type coep-aufnr,
      soma  type coep-wtgbtr,
    end of ty_coep_sum.

  constants: c_tolerancia type coep-wtgbtr value '0.01'.

  data: c_30(2)     type c value 30,
        c_koao(4)   type c value 'KOAO',
        lt_coep_sum type table of ty_coep_sum,
        wa_coep_sum type ty_coep_sum.

  data(lv_perio) = p_perio.
  free: lt_pendentes.

  "Seleciona area contabilização com base na empresa.
  select single kokrs from tka02 where bukrs = @p_data-bukrs into @data(lv_kokrs)."149202 Corrigir execução do job de liquidação de ordens - PSA


  if p_anterior is not initial. "Clique no botão periodo anterior

    data(lv_mes) = sy-datum+4(2).

    if lv_mes = '01'.

      lv_perio = '012'.
      p_gjahr = p_gjahr - 1.

    else.
      lv_perio = lv_mes - 1.
    endif.
  endif.

*   Agregação de custos e liquidação por ordem
  select
    coep~aufnr,
    coep~gjahr,
    coep~perio,
    sum( case when coep~beknz = 'S' then coep~wtgbtr else 0 end ) as total_custos,
    sum( case
            when coep~gkoar in ('A', 'K')
             and coep~objnr like 'OR%'
             and coep~wtgbtr < 0
         then coep~wtgbtr else 0 end ) as total_liquidado
  from coep
  where coep~bukrs = @p_data-bukrs
    and coep~kokrs = @lv_kokrs
    and coep~perio = @lv_perio
    and coep~gjahr = @p_gjahr
    and coep~autyp = @c_30
    and coep~objnr like 'OR%'
  group by coep~aufnr, coep~gjahr, coep~perio
  into table @lt_saldos.

  if sy-subrc eq 0.
    loop at lt_saldos assigning field-symbol(<ls_saldo>).
      if ( <ls_saldo>-total_custos + <ls_saldo>-total_liquidado ) > c_tolerancia.
        append value ty_pendente(
          aufnr = <ls_saldo>-aufnr
          gjahr = <ls_saldo>-gjahr
          perio = <ls_saldo>-perio
        ) to lt_pendentes.
      endif.
    endloop.

    free: gt_coep.
    select * from coep
    into table @gt_coep
    for all entries in @lt_pendentes
    where aufnr eq @lt_pendentes-aufnr.

*  FREE: gt_coep.
*  SELECT * FROM coep
*    INTO TABLE @gt_coep
*  WHERE bukrs = @p_data-bukrs
*    AND kokrs = @lv_kokrs
*    AND perio = @lv_perio
*    AND gjahr = @p_gjahr
*    AND autyp = @c_30.


*** Inicio - Rubenilson - 18.12.24 - US139217
*    loop at gt_coep assigning field-symbol(<fs_coep>).
*      wa_coep_sum-aufnr = <fs_coep>-aufnr.
*      wa_coep_sum-soma  = <fs_coep>-wtgbtr.
*      collect wa_coep_sum into lt_coep_sum.
*    endloop.

*    loop at lt_coep_sum assigning field-symbol(<fs_coep_sum>).
*      if <fs_coep_sum>-soma <= 0.
*        delete gt_coep where aufnr = <fs_coep_sum>-aufnr.
*      endif.
*    endloop.
*** Fim - Rubenilson - 18.12.24 - US139217

    data(lt_coep) = gt_coep.
    sort lt_coep by belnr.
    delete adjacent duplicates from lt_coep comparing belnr.

    sort gt_coep by vrgng.
    delete gt_coep where vrgng = c_koao.

    sort gt_coep ascending.
    delete adjacent duplicates from gt_coep.
  endif.

  if gt_coep[] is initial.
    message 'Ordens a liquidar não encontradas para este Período/Empresa' type 'S' display like 'I'.
  endif.

  clear: lv_kokrs.
endform.

*FORM display_data CHANGING p_data TYPE tab_data
*                            p_alv  TYPE REF TO  cl_salv_table.
*
*
*  CHECK gt_data[] IS NOT INITIAL.
*
*
*  DATA:
*    lv_text    TYPE        string,
*    lv_tooltip TYPE        string.
*
*  DATA:
*    lr_layout   TYPE REF TO cl_salv_layout.
*
*  DATA:
*    ls_key      TYPE        salv_s_layout_key.
*
*
*  TRY.
*      cl_salv_table=>factory( EXPORTING r_container  = cl_gui_container=>default_screen
*                              IMPORTING r_salv_table = p_alv
*                              CHANGING  t_table      = p_data ).
*    CATCH cx_root.
**   maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
*  TRY.
**      enable buttons for SALV
*      p_alv->get_functions( )->set_all( abap_true ).
*
*
*      CLEAR: lv_text, lv_tooltip.
*
*      lv_text     = 'Atualizar Status Job'.
**      lv_tooltip  = ''.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_refresh_job }|
*                                             icon     = |{ gc_icon_refresh  }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      lv_text     = 'Iniciar Job'.
*      lv_tooltip  = 'Cria job - Período atual'.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job }|
*                                             icon     = |{ gc_background_job }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lv_text     = 'Iniciar Job ( período anterior)'.
*      lv_tooltip  = 'Cria job - Período anterior'.
*
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_start_job_ant }|
*                                             icon     = |{ gc_background_job_ant }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CLEAR: lv_text, lv_tooltip.
*
*      lv_text     = 'Cancelar Job'.
**     lv_tooltip  = 'Botão 2 dica'.
*
*      p_alv->get_functions( )->add_function( name     = |{ gc_btn_close_job }|
*                                             icon     = |{ gc_terminated_job }|
*                                             text     = lv_text
*                                             tooltip  = lv_tooltip
*                                             position = if_salv_c_function_position=>right_of_salv_functions ).
***********************************************************************145948 - Ajuste para visualizar erros ZPM0106 - PSA
**      CLEAR: lv_text, lv_tooltip.
**
**      lv_text     = 'Erros'.
**      lv_tooltip  = 'Erros de KO8G'.
**
**      p_alv->get_functions( )->add_function( name     = |ERROS|
**                                             icon     = |@UI@|
**                                             text     = lv_text
**                                             tooltip  = lv_tooltip
**                                             position = if_salv_c_function_position=>right_of_salv_functions ).
***********************************************************************
**      set event handler
*      SET HANDLER lcl_events=>on_toolbar_click FOR p_alv->get_event( ).
*
*    CATCH cx_root.
**     maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
*
*  TRY.
*
*      p_alv->get_columns( )->get_column( 'ICON' )->set_long_text( 'Stat.Job' ).
*      p_alv->get_columns( )->get_column( 'ICON' )->set_medium_text( 'Status Job' ).
*      p_alv->get_columns( )->get_column( 'ICON' )->set_short_text( 'Status Job' ).
*
*      p_alv->get_columns( )->set_optimize( abap_true ).
*
*    CATCH cx_root.
**     maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
*  TRY.
*
*      p_alv->get_columns( )->get_column( 'DESC' )->set_long_text( 'Descrição' ).
*      p_alv->get_columns( )->get_column( 'DESC' )->set_medium_text( 'Desc.' ).
*      p_alv->get_columns( )->get_column( 'DESC' )->set_short_text( 'Desc.' ).
*
*      p_alv->get_columns( )->set_optimize( abap_true ).
*
*    CATCH cx_root.
**     maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
*
*
*  TRY.
*
*      lr_layout = p_alv->get_layout( ).
*
**     set the Layout Key
*      ls_key-report = sy-repid.
*      lr_layout->set_key( ls_key ).
*
**     set usage of default Layouts
*      lr_layout->set_default( abap_true ).
*
**      p_alv->get_selections( )->set_selection_mode( cl_salv_selections=>multiple ).
*      p_alv->get_selections( )->set_selection_mode( cl_salv_selections=>row_column ).
*    CATCH cx_root.
**     maybe some errorhandling here - just haven't made up my mind yet
*  ENDTRY.
*
**  display the SALV
*  p_alv->display( ).
*
*  WRITE: space.
*
*
*ENDFORM.
form start_job_manual using p_anterior type c changing p_data  type tab_data. "149202 Corrigir execução do job de liquidação de ordens - PSA

  types: begin of ty_list,
           bukrs type bukrs,
         end of ty_list.

  data: it_list type standard table of ty_list initial size 0.
  free: it_list.
  select single low from tvarvc into @data(l_list) where name = 'ZPM0106' .

  if sy-subrc = 0.
    split l_list at ';' into table it_list.
    sort it_list ascending.
    delete adjacent duplicates from it_list.

    data: lt_return type table of ddshretval,
          ls_return type ddshretval.

    clear: lt_return[],ls_return.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'BUKRS'
        value_org       = 'S'
        window_title    = 'Seleção de Empresas'
      tables
        value_tab       = it_list
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    read table lt_return into ls_return index 1.
    if ls_return-fieldval is not initial.
      data(l_bukrs) = ls_return-fieldval.
      read table p_data assigning field-symbol(<data>) with key bukrs = l_bukrs.
      "PERFORM valida_job USING <data>-bukrs.
      perform selecionar_dados using <data> p_anterior.
      check gt_coep[] is not initial .
      perform criar_variant using gc_prog_variant.
      check lv_variant is not initial and lv_jobname is not initial.
      perform criar_job using <data>.

    endif.
  endif.
endform.

form open_job       using p_rows  type salv_t_row
                          p_anterior type c
                    changing p_data  type tab_data.


  data:
    lv_subrc  type sy-subrc.



  loop at p_rows assigning field-symbol(<row>).

    read table p_data assigning field-symbol(<data>) index <row>.

    if sy-subrc = 0.

      "PERFORM valida_job USING <data>-bukrs.
      perform selecionar_dados using <data>
                                     p_anterior.

      if sy-sysid <> 'DEV'.
        check gt_coep[] is not initial .
      endif.

      check gt_coep[] is not initial .
      perform criar_variant using gc_prog_variant.
      check lv_variant is not initial and lv_jobname is not initial.
      perform criar_job using <data>.

      clear:
        lv_subrc.

    endif.
  endloop.

endform.

form open_job_manual using p_anterior type c changing p_data  type tab_data. "149202 Corrigir execução do job de liquidação de ordens - PSA


  types: begin of ty_list,
           bukrs type bukrs,
         end of ty_list.

  data: it_list type standard table of ty_list initial size 0.
  free: it_list.
  select single low from tvarvc into @data(l_list) where name = 'ZPM0106' .

  if sy-subrc = 0.
    split l_list at ';' into table it_list.
    sort it_list ascending.
    delete adjacent duplicates from it_list.

    data: lt_return type table of ddshretval,
          ls_return type ddshretval.

    clear: lt_return[],ls_return.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'BUKRS'
        value_org       = 'S'
        window_title    = 'Seleção de Empresas'
      tables
        value_tab       = it_list
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    read table lt_return into ls_return index 1.
    if ls_return-fieldval is not initial.
      data(l_bukrs) = ls_return-fieldval.
      read table p_data assigning field-symbol(<data>) with key bukrs = l_bukrs.
      "PERFORM valida_job USING <data>-bukrs.
      perform selecionar_dados using <data> p_anterior.
      check gt_coep[] is not initial .
      perform criar_variant using gc_prog_variant.
      check lv_variant is not initial and lv_jobname is not initial.
      perform criar_job using <data>.
    endif.
  endif.

endform.

form close_job    using p_rows  type salv_t_row
                    changing p_data  type tab_data.


  data:
    lv_subrc  type sy-subrc.



  loop at p_rows assigning field-symbol(<row>).

    read table p_data assigning field-symbol(<data>) index <row>.

    if sy-subrc = 0.

      data(lv_jobname) = |{ gc_prog_exec && <data>-bukrs }|.
      read table gt_job into data(wa_job) with key name = lv_jobname.
      if sy-subrc = 0.

        perform cancela_job using wa_job-name
                                  wa_job-low
                                  <data>-bukrs.

      endif.

      clear:
        lv_subrc.

    endif.
  endloop.

endform.

form close_job_manual  changing p_data  type tab_data. "149202 Corrigir execução do job de liquidação de ordens - PSA


  types: begin of ty_list,
           bukrs type bukrs,
         end of ty_list.

  data: it_list type standard table of ty_list initial size 0.
  free: it_list.
  select single low from tvarvc into @data(l_list) where name = 'ZPM0106' .

  if sy-subrc = 0.
    split l_list at ';' into table it_list.
    sort it_list ascending.
    delete adjacent duplicates from it_list.

    data: lt_return type table of ddshretval,
          ls_return type ddshretval.

    clear: lt_return[],ls_return.

    call function 'F4IF_INT_TABLE_VALUE_REQUEST'
      exporting
        retfield        = 'BUKRS'
        value_org       = 'S'
        window_title    = 'Seleção de Empresas'
      tables
        value_tab       = it_list
        return_tab      = lt_return
      exceptions
        parameter_error = 1
        no_values_found = 2
        others          = 3.

    read table lt_return into ls_return index 1.
    if ls_return-fieldval is not initial.
      data(l_bukrs) = ls_return-fieldval.
      read table p_data assigning field-symbol(<data>) with key bukrs = l_bukrs.

      data(lv_jobname) = |{ gc_prog_exec && <data>-bukrs }|.
      read table gt_job into data(wa_job) with key name = lv_jobname.
      if sy-subrc = 0.

        perform cancela_job using wa_job-name
                                  wa_job-low
                                  <data>-bukrs.
      endif.

    endif.
  endif.

endform.


form criar_job using p_data type lin_data.

  data: "lv_jobname          TYPE btcjob,  " Nome do job
    lv_jobcount         type tbtcjob-jobcount,               " Identificador do job
    lv_startdate        type d,
    lv_starttime        type t,
    lv_enddate          type d,
    lv_endtime          type t,
    ls_tvarvc           type tvarvc,
    ls_spool_parameters type pri_params.

  data: lv_ano            type numc4,
        lv_mes            type numc2,
        lv_ultimo_dia_mes type endda,
        lv_qtd_dias       type tbtcjob-prddays.
  "lv_variant        TYPE raldb-variant.

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



  "PERFORM criar_variant USING gc_prog_variant.


*  CONCATENATE gc_prog_exec p_data-bukrs INTO lv_jobname.
*  CONCATENATE gc_liquida p_data-bukrs INTO lv_variant.

  " Criar um job
  call function 'JOB_OPEN'
    exporting
      jobname          = lv_jobname
    importing
      jobcount         = lv_jobcount
    exceptions
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      others           = 4.



  if sy-subrc <> 0.
    message 'Erro ao criar o job' type 'E'.
    check sy-subrc <> 0.
    "EXIT.
  endif.



  data: testlauf  type xfeld,
        c_magi(4) type c value 'MAGI'.

  set parameter id 'CAC' field c_magi.
  set parameter id 'ORV' field lv_variant.


  if p_data-bukrs is not initial.

    select single kokrs from tka02 where bukrs = @p_data-bukrs into @data(lv_kokrs)."149202 Corrigir execução do job de liquidação de ordens - PSA

    submit rko7ko8g "149202 Corrigir execução do job de liquidação de ordens - PSA
           user sy-uname
           with kokrs = lv_kokrs
           with variant = lv_variant
           with perio = p_perio
           with gjahr = p_gjahr
           "WITH vaart = '1'
           with ausf = abap_true
           with testlauf = abap_false
           to sap-spool
           "SPOOL PARAMETERS ls_spool_parameters
           without spool dynpro
           via job lv_jobname number lv_jobcount
           and return.



    if sy-subrc <> 0.
      message 'Erro ao adicionar passo ao job' type 'E'.
      check sy-subrc <> 0.
*      EXIT.
    endif.
  endif.

  " Fechar e executar imediatamente
  call function 'JOB_CLOSE'
    exporting
      jobcount             = lv_jobcount
      jobname              = lv_jobname
      strtimmed            = abap_true
*     sdlstrtdt            = lv_startdate
*     sdlstrttm            = lv_starttime
*     prddays              = lv_qtd_dias  "Quantidade de dias que será executado
    exceptions
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      others               = 9.



  if sy-subrc <> 0.
    message 'Erro ao fechar o job' type 'E'.
    check sy-subrc <> 0.
*    EXIT.
  else.

    if sy-batch is initial.
      concatenate gc_prog_exec p_data-bukrs into lv_jobname.

      ls_tvarvc-low  = lv_jobcount.
      ls_tvarvc-name = lv_jobname.

      " Verificar se a variável já existe na tabela
      select *
      from tvarvc
      into table @data(dummy)
      where name = @ls_tvarvc-name.

      if sy-subrc = 0.

        " Atualizar o registro existente
        update tvarvc set  low = lv_jobcount
                    where name = lv_jobname.

      else.
        " Inserir novo registro
        insert into tvarvc values ls_tvarvc.
      endif.

      read table gt_data assigning field-symbol(<fs_data>) with key bukrs = p_data-bukrs.
      if sy-subrc = 0.
        <fs_data>-icon = gc_light_yellow.
        <fs_data>-desc = 'Em execução'.
      endif.

*      MESSAGE 'Job criado e será executado imediatamente' TYPE 'S'.
    endif.
  endif.


endform.

form criar_variant using p_prog.

  clear: lv_jobname, lv_variant.

  clear: lt_variant,ls_vari_desc,lt_vari_text[], lt_vari_contents[], gv_set_erro.

  append initial line to lt_vari_text assigning field-symbol(<fs_vari_text>).


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
  loop at gt_coep into data(wa_coep).

    concatenate gc_liquida wa_coep-bukrs into ls_vari_desc-variant.
    <fs_vari_text>-variant = ls_vari_desc-variant.

    append initial line to lt_vari_contents assigning field-symbol(<fs_vari_contents>).
    <fs_vari_contents>-selname  = 'AUFNR'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-aufnr.

    append initial line to lt_vari_contents assigning <fs_vari_contents>.
    <fs_vari_contents>-selname  = 'BUKRS'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-bukrs.

    append initial line to lt_vari_contents assigning <fs_vari_contents>.
    <fs_vari_contents>-selname  = 'PERIO'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-perio.

    append initial line to lt_vari_contents assigning <fs_vari_contents>.
    <fs_vari_contents>-selname  = 'SVALD-VALUE'.
    <fs_vari_contents>-kind     = 'S'.
    <fs_vari_contents>-sign     = 'I'.
    <fs_vari_contents>-option   = 'EQ'.
    <fs_vari_contents>-low      = wa_coep-kokrs.
  endloop.

  " Criar a variante

  sort lt_vari_contents by selname ascending low ascending.

  delete adjacent duplicates from lt_vari_contents comparing all fields.

  if lt_vari_contents is not initial.
    data: lv_curr_report  type rsvar-report,
          lv_curr_variant type rsvar-variant,
          lv_message      type char45,
          rc              like  sy-subrc.

    lv_curr_report = p_prog.

    concatenate gc_liquida wa_coep-bukrs into lv_curr_variant.

*Check variant
    call function 'RS_VARIANT_EXISTS'
      exporting
        report              = lv_curr_report
        variant             = lv_curr_variant
      importing
        r_c                 = rc
      exceptions
        not_authorized      = 01
        no_report           = 02
        report_not_existent = 03
        report_not_supplied = 04.

*    if sy-subrc <> 0.
*      lv_message = 'Variante' && lv_curr_variant && não encontrada.
*      message lv_message(e26) type 'I' display like 'E'.
*    endif.

    if rc = 0.                    " Variante existiert
      call function 'RS_CHANGE_CREATED_VARIANT'
        exporting
          curr_report               = lv_curr_report
          curr_variant              = lv_curr_variant
          vari_desc                 = ls_vari_desc
        tables
          vari_contents             = lt_vari_contents
          vari_text                 = lt_vari_text
        exceptions
          illegal_report_or_variant = 01
          illegal_variantname       = 02
          not_authorized            = 03
          not_executed              = 04
          report_not_existent       = 05
          report_not_supplied       = 06
          variant_doesnt_exist      = 07
          variant_locked            = 08
          selections_no_match       = 09.

      if sy-subrc <> 0.
        message 'Erro ao alterar a variante'(e24) type 'I' display like 'E'.
        gv_set_erro = abap_true.
        exit.
      else.
        commit work and wait."149202 Corrigir execução do job de liquidação de ordens - PSA
      endif.

    else.

      call function 'RS_CREATE_VARIANT'
        exporting
          curr_report               = lv_curr_report
          curr_variant              = lv_curr_variant
          vari_desc                 = ls_vari_desc
*         P_XML_TAB                 =
*         SUPPRESS_AUTHORITY_CHECK  =
        tables
          vari_contents             = lt_vari_contents
          vari_text                 = lt_vari_text
*         VSCREENS                  =
*         VARI_CONTENTS_L           =
        exceptions
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_exists            = 7
          variant_locked            = 8
          others                    = 9.

      if sy-subrc <> 0.
        message 'Erro ao criar a variante'(e25) type 'I' display like 'E'.
        gv_set_erro = abap_true.
        exit.
      else.
        commit work and wait."149202 Corrigir execução do job de liquidação de ordens - PSA
      endif.
    endif.

    concatenate gc_prog_exec wa_coep-bukrs into lv_jobname.
    concatenate gc_liquida wa_coep-bukrs into lv_variant.
  endif.
endform.


form cancela_job using p_jobname
                       p_jobcount
                       p_bukrs.


  data: iv_jobname  type  btcjob,
        iv_jobcount type  btcjobcnt.


  iv_jobname = p_jobname.
  iv_jobcount = p_jobcount.

  " Cancelar o job

  call function 'SLTR_JOB_CANCEL'
    exporting
      iv_jobname  = iv_jobname
      iv_jobcount = iv_jobcount
    exceptions
      error       = 1
      others      = 2.

  if sy-subrc <> 0.
    message 'Erro ao cancelar o job' type 'E'.
  else.

    delete from tvarvc where name = p_jobname
                         and low  = p_jobcount.

    read table gt_data assigning field-symbol(<fs_data>) with key bukrs = p_bukrs.
    if sy-subrc = 0.
      <fs_data>-icon = gc_light_red.
    endif.

    message 'Job cancelado com sucesso' type 'S'.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form valida_job
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <DATA>
*&---------------------------------------------------------------------*
form valida_job  using    p_bukrs.

  data: lv_jobname type tvarvc-name.

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

endform.

form f_status_job using p_jobcount
                        p_jobname
                  changing p_status.

  data: l_jobname  type tbtco-jobname,
        l_jobcount type tbtco-jobcount.

  l_jobname = p_jobname .
  l_jobcount = p_jobcount.


  data:
    aborted     like  tbtcv-abort,
    finished    like  tbtcv-fin,
    preliminary like  tbtcv-prelim,
    ready       like  tbtcv-ready,
    running     like  tbtcv-run,
    scheduled   like  tbtcv-sched,
    suspended   type  btcstatus,
    other       type  btcstatus.

  call function 'SHOW_JOBSTATE'
    exporting
      jobcount         = l_jobcount
      jobname          = l_jobname
    importing
      aborted          = aborted
      finished         = finished
      preliminary      = preliminary
      ready            = ready
      running          = running
      scheduled        = scheduled
      suspended        = suspended
      other            = other
    exceptions
      jobcount_missing = 1
      jobname_missing  = 2
      job_notex        = 3
      others           = 4.
  if sy-subrc <> 0.
* Implement suitable error handling here
  endif.

  if aborted is not initial.
    p_status = 'Abortado'.

  elseif finished is not initial.
    p_status = 'Finalizado'.

  elseif preliminary is not initial.
    p_status = 'Preliminar'.

  elseif ready is not initial.
    p_status = 'Pronto'.

  elseif running is not initial.
    p_status = 'Em execução'.

  elseif scheduled is not initial.
    p_status = 'Agendado'.

  elseif suspended is not initial.
    p_status = 'suspenso'.

  elseif other is not initial.
    p_status = 'Outros'.
  endif.

endform.

form f_refresh.

  select * from tvarvc where name = 'ZPM0106' into table @data(lt_tvarv).
  if sy-subrc = 0.
    loop at lt_tvarv assigning field-symbol(<fs_tvarv>).
      split <fs_tvarv>-low at ';' into table lt_param.
      append lt_param.
    endloop.

    lr_bukrs = value #( for ls_value in lt_param ( sign = 'I'
                                                 option = 'EQ'
                                                 low = ls_value-low ) ).
  endif.

  clear gt_job.

  select * from tvarvc
   into table gt_job
   where name like 'RKO7KO8G%'.

  if sy-subrc <> 0.
    clear: gt_job, gt_job[].
  endif.

  clear: gt_data, gt_data[].

  loop at lr_bukrs into data(wa_bukrs).
    check wa_bukrs-low is not initial.

    append initial line to s_bukrs assigning field-symbol(<fs_bukrs>).
    <fs_bukrs>-sign = 'I'.
    <fs_bukrs>-option = 'EQ'.

    append initial line to gt_data assigning field-symbol(<fs_data>).
    <fs_bukrs>-low = wa_bukrs-low.
    <fs_data>-bukrs = wa_bukrs-low.
    <fs_data>-perio = p_perio.
    <fs_data>-gjahr = p_gjahr.

    data(lv_jobname) = |{ gc_prog_exec && wa_bukrs-low }| .

    read table gt_job with key name = lv_jobname into data(wa).

    if sy-subrc <> 0.
      <fs_data>-icon = gc_light_red.
    else.

      perform f_status_job using wa-low
                                 lv_jobname
                        changing gv_status.

      <fs_data>-desc = gv_status.

      case gv_status.

        when 'Finalizado'.
          <fs_data>-icon = gc_light_green.

        when 'Abortado' or ' '.
          <fs_data>-icon = gc_light_red.

        when others.
          <fs_data>-icon = gc_light_yellow.

      endcase.

    endif.

  endloop.

endform.
