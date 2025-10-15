*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.       *
*****************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                      *
* Data desenv ...: 17.07.2012                                               *
* Objetivo    ...: Liberação de Credito no Cartão TIPFRETE                  *
* Transação   ...: ZMEM010                                                  *
* Autor       ...: Victor Hugo                                              *
*****************************************************************************

report  zlesr0021.

*****************************************************************************
* Tables
*****************************************************************************
tables: zlest0046, lfa1.

*****************************************************************************
* TYPES
*****************************************************************************

types: begin of ty_zlest0046,

          mandt       type  zlest0046-mandt,
          lifnr       type  zlest0046-lifnr,
          route       type  zlest0046-route,
          dt_inicio   type  zlest0046-dt_inicio ,
          dt_fim      type  zlest0046-dt_fim,
          st_adto     type  zlest0046-st_adto,
          st_saldo    type  zlest0046-st_saldo,
          status      type  zlest0046-status,
          observacao  type  zlest0046-observacao,

       end of ty_zlest0046,

       begin of ty_lfa1,
          lifnr type lfa1-lifnr,
          name1 type lfa1-name1,
       end of ty_lfa1,

       begin of ty_saida,
          opcao       type  c length 4,
          status      type  c length 4,
          lifnr       type  zlest0046-lifnr,
          route       type  zlest0046-route,
          dt_inicio   type  zlest0046-dt_inicio ,
          dt_fim      type  zlest0046-dt_fim,
          st_adto     type  c length 4,
          st_saldo    type  c length 4,



          observacao  type  zlest0046-observacao,
          name1        type  lfa1-name1,

       end of ty_saida.

*****************************************************************************
* Internal Table
*****************************************************************************
data: it_zlest0046 type table of ty_zlest0046,
      it_lfa1      type table of ty_lfa1,
      it_saida     type table of ty_saida.

*****************************************************************************
* Work Area
*****************************************************************************
data: wa_zlest0046 type ty_zlest0046,
      wa_liberacao type ty_zlest0046,
      wa_lfa1      type ty_lfa1,
      wa_saida     type ty_saida,
      wa_stable    type lvc_s_stbl.

*****************************************************************************
* Constants
*****************************************************************************
constants: tela_01(4) type c value 0100,
           tela_02(4) type c value 0200,
           tela_03(4) type c value 0300.


*****************************************************************************
* ALV
*****************************************************************************
data: cl_container    type ref to cl_gui_custom_container,
      cl_grid         type ref to cl_gui_alv_grid,
      wa_fieldcatalog type lvc_s_fcat,
      it_fieldcatalog type lvc_t_fcat.

selection-screen: begin of block b1 with frame title text-001.
select-options: p_lifnr for zlest0046-lifnr.
selection-screen: end of block b1.

start-of-selection.

  perform: seleciona_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
form seleciona_dados .

  select  mandt lifnr route dt_inicio dt_fim st_adto st_saldo status observacao
    from zlest0046
    into table it_zlest0046
  where lifnr in p_lifnr.

  select
    lifnr
    name1
    from lfa1
    into table it_lfa1
    for all entries in it_zlest0046
  where lifnr eq it_zlest0046-lifnr.

  loop at it_zlest0046 into wa_zlest0046.

    wa_saida-opcao      = icon_change.

    if ( wa_zlest0046-status eq 'A' ).
      wa_saida-status     = icon_led_green.
    elseif ( wa_zlest0046-status eq 'D' ) .
      wa_saida-status     = icon_led_red.
    endif.

    wa_saida-lifnr      = wa_zlest0046-lifnr.
    wa_saida-route      = wa_zlest0046-route.
    wa_saida-dt_inicio  = wa_zlest0046-dt_inicio.
    wa_saida-dt_fim     = wa_zlest0046-dt_fim.

    case wa_zlest0046-st_adto.
      when: 'S'.
        wa_saida-st_adto = 'Sim'.
      when: 'N'.
        wa_saida-st_adto = 'Não'.
    endcase.

    case wa_zlest0046-st_saldo.
      when: 'S'.
        wa_saida-st_saldo = 'Sim'.
      when: 'N'.
        wa_saida-st_saldo = 'Não'.
    endcase.

    wa_saida-observacao = wa_zlest0046-observacao.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_zlest0046-lifnr.

    if ( sy-subrc eq 0 ).

      wa_saida-name1 = wa_lfa1-name1.

    endif.

    append wa_saida to it_saida.

    clear: wa_saida, wa_zlest0046, wa_lfa1.

  endloop.

  call screen tela_01.

endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
module pbo output.
  set pf-status 'PF0100'.
  set titlebar  'TB0100'.

  if ( cl_container is initial ).
    perform: criar_objeto.
  endif.


endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
module pai input.

  case sy-ucomm.
    when: 'BACK'.
      leave to screen 0.
    when: 'CANC'.
      leave to screen 0.
    when: 'EXIT'.
      leave program.
    when: 'NOVO'.

      perform: liberacao_credito.

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  LIBERACAO_CREDITO
*&---------------------------------------------------------------------*
form liberacao_credito .
  clear: wa_liberacao.
  call screen tela_02 starting at 45 5 ending at 95 15.

endform.                    " LIBERACAO_CREDITO
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_handler definition.

  public section.
    methods: handle_hotspot_click for event hotspot_click of cl_gui_alv_grid importing e_row_id e_column_id es_row_no.
endclass.                    "lcl_event_handler DEFINITION

*&---------------------------------------------------------------------*
*&      CLASS IMPLMENTATION
*&---------------------------------------------------------------------*
class lcl_event_handler implementation.

  method handle_hotspot_click.
    perform handle_hotspot_click using e_row_id e_column_id es_row_no.
  endmethod.                    "handle_hotspot_click


endclass.                    "lcl_event_handler IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
form handle_hotspot_click  using    i_row_id     type lvc_s_row
                                    i_column_id  type lvc_s_col
                                    is_row_no    type lvc_s_roid.
  case i_column_id.
    when: 'OPCAO'.
      perform: editar_liberacao using is_row_no-row_id.

    when: 'STATUS'.
      perform: ativar_liberacao using is_row_no-row_id.
  endcase.
endform.                    " HANDLE_HOTSPOT_CLICK


*&---------------------------------------------------------------------*
*&      Form  CRIAR_OBJETO
*&---------------------------------------------------------------------*
form criar_objeto .

  data:       wa_layout        type lvc_s_layo,
              gs_variant       type disvariant,
              gr_event_handler type ref to lcl_event_handler.


  wa_stable-row        = 'X'.

  create object cl_container
    exporting
      container_name = 'CONTAINER_PRINCIPAL'.

  create object cl_grid
    exporting
      i_parent = cl_container.

  perform: criar_catalog.

  create object gr_event_handler.
  set handler gr_event_handler->handle_hotspot_click for cl_grid.


  call method cl_grid->set_table_for_first_display
    exporting
      is_layout                     = wa_layout
      is_variant                    = gs_variant
    changing
      it_outtab                     = it_saida[]
      it_fieldcatalog               = it_fieldcatalog[]
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4.



endform.                    " CRIAR_OBJETO
*&---------------------------------------------------------------------*
*&      Form  CRIAR_CATALOG
*&---------------------------------------------------------------------*
form criar_catalog .

  perform fieldcatalog using:

  'OPCAO'       'Opção'            '6'  ''  'X' ''  'C' '',
  'STATUS'      'Status'           '4'  ''  'X' ''  ''  '',
  'LIFNR'       'Cod. Fornecedor'  '11' 'X' ''  '' ''   '',
  'NAME1'       'Fornecedor'       '18' ''  ''  ''  ''  '',
  'ROUTE'       'Itinerário'       '7'  ''  ''  ''  ''  '',
  'DT_INICIO'   'Validade Inicial' '15' ''  ''  ''  ''  '',
  'DT_FIM'      'Validade Final'   '15' ''  ''  ''  ''  '',
  'ST_ADTO'     'Status Adto.'     '10' ''  ''  ''  ''  '',
  'ST_SALDO'    'Status Saldo'     '10' ''  ''  ''  ''  '',
  'OBSERVACAO'  'Observação'       '50' ''  ''  ''  ''  ''.

endform.                    " CRIAR_CATALOG
*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*

form fieldcatalog  using  value(p_fieldname)
                          value(p_desc)
                          value(p_tam)
                          value(p_no_zero)
                          value(p_hotspot)
                          value(p_cor)
                          value(p_just)
                          value(p_sum).

  wa_fieldcatalog-fieldname = p_fieldname.
  wa_fieldcatalog-scrtext_l = p_desc.
  wa_fieldcatalog-scrtext_m = p_desc.
  wa_fieldcatalog-scrtext_s = p_desc.
  wa_fieldcatalog-outputlen = p_tam.
  wa_fieldcatalog-no_zero   = p_no_zero.
  wa_fieldcatalog-hotspot   = p_hotspot.
  wa_fieldcatalog-emphasize = p_cor.
  wa_fieldcatalog-just      = p_just.
  wa_fieldcatalog-do_sum    = p_sum.

  append wa_fieldcatalog to it_fieldcatalog.

  clear: wa_fieldcatalog.

endform.                    " FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
module pbo_0200 output.
  set pf-status 'PF0200'.
  set titlebar  'TB0200'.

endmodule.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
module pai_0200 input.



  if ( sy-dynnr eq 0200 ).

    case sy-ucomm.
      when: 'CANC'.
        leave to screen 0.
      when: 'OK'.

        if ( wa_liberacao-dt_inicio > wa_liberacao-dt_fim ).
          message s888(sabapdocu) with 'Data inicio menor do que data fim' display like 'W'.

        elseif ( wa_liberacao-lifnr is initial ).
          message s888(sabapdocu) with 'Informar o fornecedor' display like 'W'.
        else.

          perform: gravar_liberacao.

        endif.


    endcase.

  endif.

endmodule.                 " USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*&      Form  GRAVAR_LIBERACAO
*&---------------------------------------------------------------------*
form gravar_liberacao .

  data: zlest0046_wa type zlest0046,
        wa_lfa1      type lfa1.

  data: aux_lifnr type lfa1-lifnr.

  data: it_range type table of ty_zlest0046,
        wa_range type ty_zlest0046,
        erro      type c.


  if not ( wa_liberacao is initial ).


    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_liberacao-lifnr
      importing
        output = aux_lifnr.


    select  mandt lifnr route dt_inicio dt_fim st_adto st_saldo status observacao
      from zlest0046
      into table it_range
    where lifnr eq aux_lifnr.


    loop at it_range into wa_range.


      if ( wa_range-route eq wa_liberacao-route ) and ( wa_range-lifnr eq wa_liberacao-lifnr ).

        if ( wa_liberacao-dt_inicio >= wa_range-dt_inicio ) and ( wa_liberacao-dt_inicio <= wa_range-dt_fim ) and ( wa_range-status ne 'D' ).
          message s888(sabapdocu) with 'Range já cadastrado!' display like 'W'.

          erro = 'X'.

          leave to screen 0.
        endif.


        if ( wa_liberacao-dt_inicio >= wa_range-dt_inicio ) and ( wa_liberacao-dt_inicio <= wa_range-dt_fim )  and ( wa_range-status ne 'D' ).
          message s888(sabapdocu) with 'Range já cadastrado!' display like 'W'.

          erro = 'X'.
          leave to screen 0.
        endif.


        if ( wa_liberacao-dt_fim >= wa_range-dt_inicio ) and ( wa_liberacao-dt_fim <= wa_range-dt_fim )  and ( wa_range-status ne 'D' ).
          message s888(sabapdocu) with 'Range já cadastrado!' display like 'W'.

          erro = 'X'.
          leave to screen 0.
        endif.

      endif.

      if ( wa_liberacao-route eq '' ) and ( wa_range-status eq 'A' ) and ( wa_liberacao-dt_inicio >= wa_range-dt_inicio ) and ( wa_liberacao-dt_inicio <= wa_range-dt_fim ) and ( wa_range-status ne 'D' ).
        message s888(sabapdocu) with 'Itinerario' display like 'W'.

        erro = 'X'.

        leave to screen 0.
      endif.


      if ( wa_liberacao-route eq '' ) and ( wa_range-status eq 'A' ) and ( wa_liberacao-dt_inicio >= wa_range-dt_inicio ) and ( wa_liberacao-dt_inicio <= wa_range-dt_fim )  and ( wa_range-status ne 'D' ).
        message s888(sabapdocu) with 'Itinerario' display like 'W'.

        erro = 'X'.
        leave to screen 0.
      endif.


      if ( wa_liberacao-route eq '' ) and ( wa_range-status eq 'A' ) and ( wa_liberacao-dt_fim >= wa_range-dt_inicio ) and ( wa_liberacao-dt_fim <= wa_range-dt_fim )  and ( wa_range-status ne 'D' ).
        message s888(sabapdocu) with 'Itinerario' display like 'W'.

        erro = 'X'.
        leave to screen 0.
      endif.








*      IF ( WA_RANGE-ROUTE NE WA_LIBERACAO-ROUTE ) AND ( WA_RANGE-STATUS EQ 'A' ) AND ( WA_LIBERACAO-DT_FIM <= WA_RANGE-DT_FIM ).
*
*        MESSAGE S888(SABAPDOCU) WITH 'Itinerario ativo' DISPLAY LIKE 'W'.
*
*        ERRO = 'X'.
*        LEAVE TO SCREEN 0.
*
*
*      ENDIF.


      clear: wa_range.

    endloop.


    if ( erro ne 'X' ).

      select single *
        from lfa1
        into wa_lfa1
      where lifnr eq aux_lifnr.

      if ( sy-subrc ne 0 ).

        message s888(sabapdocu) with 'Fornecedor não cadastrado.' display like 'W'.

      else.
        zlest0046_wa-status     = 'A'.
        zlest0046_wa-lifnr      = aux_lifnr.
        zlest0046_wa-route      = wa_liberacao-route.
        zlest0046_wa-dt_inicio  = wa_liberacao-dt_inicio.
        zlest0046_wa-dt_fim     = wa_liberacao-dt_fim.
        zlest0046_wa-st_adto    = wa_liberacao-st_adto.
        zlest0046_wa-st_saldo   = wa_liberacao-st_saldo.
        zlest0046_wa-observacao = wa_liberacao-observacao.

        insert into zlest0046 values zlest0046_wa.

        perform: refresh_tabela.

        clear: zlest0046_wa, wa_liberacao.

      endif.

      leave to screen 0.
    endif.
  endif.


endform.                    " GRAVAR_LIBERACAO
*&---------------------------------------------------------------------*
*&      Form  REFRESH_TABELA
*&---------------------------------------------------------------------*
form refresh_tabela .

  clear: it_saida[], wa_saida, it_zlest0046[], wa_zlest0046, it_lfa1[], wa_lfa1.

  select  mandt lifnr route dt_inicio dt_fim st_adto st_saldo status observacao
    from zlest0046
    into table it_zlest0046
  where lifnr in p_lifnr.

  select lifnr name1
    from lfa1
    into table it_lfa1
    for all entries in it_zlest0046
  where lifnr eq it_zlest0046-lifnr.

  if not ( it_zlest0046 is initial ) .

    loop at it_zlest0046 into wa_zlest0046.

      wa_saida-opcao      = icon_change.

      if ( wa_zlest0046-status eq 'A' ).
        wa_saida-status     = icon_led_green.
      elseif ( wa_zlest0046-status eq 'D' ) .
        wa_saida-status     = icon_led_red.
      endif.

      wa_saida-lifnr      = wa_zlest0046-lifnr.
      wa_saida-route      = wa_zlest0046-route.
      wa_saida-dt_inicio  = wa_zlest0046-dt_inicio.
      wa_saida-dt_fim     = wa_zlest0046-dt_fim.

      case wa_zlest0046-st_adto.
        when: 'S'.
          wa_saida-st_adto = 'Sim'.
        when: 'N'.
          wa_saida-st_adto = 'Não'.
      endcase.

      case wa_zlest0046-st_saldo.
        when: 'S'.
          wa_saida-st_saldo = 'Sim'.
        when: 'N'.
          wa_saida-st_saldo = 'Não'.
      endcase.

      wa_saida-observacao = wa_zlest0046-observacao.

      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zlest0046-lifnr.

      if ( sy-subrc eq 0 ).
        wa_saida-name1 = wa_lfa1-name1.
      endif.

      append wa_saida to it_saida.

      clear: wa_saida, wa_zlest0046.

    endloop.

    call method cl_grid->refresh_table_display
      exporting
        is_stable = wa_stable.


  endif.

endform.                    " REFRESH_TABELA
*&---------------------------------------------------------------------*
*&      Form  EDITAR_LIBERACAO
*----------------------------------------------------------------------*
form editar_liberacao  using    p_is_row_no.


  read table it_saida into wa_saida index p_is_row_no.

  if ( sy-subrc eq 0 ).

    wa_liberacao-lifnr      = wa_saida-lifnr.
    wa_liberacao-route      = wa_saida-route.
    wa_liberacao-dt_inicio  = wa_saida-dt_inicio.
    wa_liberacao-dt_fim     = wa_saida-dt_fim.
    wa_liberacao-st_adto    = wa_saida-st_adto.
    wa_liberacao-st_saldo   = wa_saida-st_saldo.
    wa_liberacao-observacao = wa_saida-observacao.

    call screen tela_03 starting at 45 5 ending at 95 15.

  endif.
endform.                    " EDITAR_LIBERACAO
*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
module pbo_0300 output.
  set pf-status 'PF0300'.
  set titlebar  'TB0300'.

endmodule.                 " STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
module pai_0300 input.

  case sy-ucomm.
    when: 'OK'.
      perform: update_liberacao.
    when: 'CANC'.
      leave to screen 0.
  endcase.

endmodule.                 " USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_LIBERACAO
*&---------------------------------------------------------------------*
form update_liberacao .

  update zlest0046 set dt_fim     = wa_liberacao-dt_fim
                       st_adto    = wa_liberacao-st_adto
                       st_saldo   = wa_liberacao-st_saldo
                       observacao = wa_liberacao-observacao
                   where lifnr     eq wa_liberacao-lifnr
                     and route     eq wa_liberacao-route
                     and dt_inicio eq wa_liberacao-dt_inicio.


  clear: it_saida[], wa_saida, it_zlest0046[], wa_zlest0046, it_lfa1[], wa_lfa1.

  select  mandt lifnr route dt_inicio dt_fim st_adto st_saldo status observacao
    from zlest0046
    into table it_zlest0046
  where lifnr in p_lifnr.

  select
    lifnr
    name1
    from lfa1
    into table it_lfa1
    for all entries in it_zlest0046
  where lifnr eq it_zlest0046-lifnr.


  if not ( it_zlest0046 is initial ) .

    loop at it_zlest0046 into wa_zlest0046.

      wa_saida-opcao      = icon_change.

      if ( wa_zlest0046-status eq 'A' ).
        wa_saida-status     = icon_led_green.
      elseif ( wa_zlest0046-status eq 'D' ) .
        wa_saida-status     = icon_led_red.
      endif.

      wa_saida-lifnr      = wa_zlest0046-lifnr.
      wa_saida-route      = wa_zlest0046-route.
      wa_saida-dt_inicio  = wa_zlest0046-dt_inicio.
      wa_saida-dt_fim     = wa_zlest0046-dt_fim.

      case wa_zlest0046-st_adto.
        when: 'S'.
          wa_saida-st_adto = 'Sim'.
        when: 'N'.
          wa_saida-st_adto = 'Não'.
      endcase.

      case wa_zlest0046-st_saldo.
        when: 'S'.
          wa_saida-st_saldo = 'Sim'.
        when: 'N'.
          wa_saida-st_saldo = 'Não'.
      endcase.
      wa_saida-observacao = wa_zlest0046-observacao.


      read table it_lfa1 into wa_lfa1 with key lifnr = wa_zlest0046-lifnr.

      if ( sy-subrc eq 0 ).
        wa_saida-name1 = wa_lfa1-name1.
      endif.

      append wa_saida to it_saida.

      clear: wa_saida, wa_zlest0046, wa_lfa1.

    endloop.


    clear: wa_saida, wa_liberacao.

    call method cl_grid->refresh_table_display
      exporting
        is_stable = wa_stable.


  endif.

  leave to screen 0.

endform.                    " UPDATE_LIBERACAO
*&---------------------------------------------------------------------*
*&      Form  ATIVAR_LIBERACAO
*&---------------------------------------------------------------------*
form ativar_liberacao  using    p_is_row_no.

  data: wa_verifica_status type zlest0046.

  read table it_saida into wa_saida index p_is_row_no.

  if ( sy-subrc eq 0 ).

    select single * from zlest0046 into wa_verifica_status where lifnr eq wa_saida-lifnr
                                                             and route eq wa_saida-route
                                                             and dt_inicio eq wa_saida-dt_inicio.

    if ( wa_verifica_status-status eq 'A' ).

      update zlest0046 set
                           status = 'D'
                       where lifnr     eq wa_saida-lifnr
                         and route     eq wa_saida-route
                         and dt_inicio eq wa_saida-dt_inicio.


      perform: refresh_tabela.


    elseif (  wa_verifica_status-status eq 'D' ).
      message s888(sabapdocu) with 'Este cadastro não pode ser ativado' display like 'W'.
    endif.

  endif.
endform.                    " ATIVAR_LIBERACAO
