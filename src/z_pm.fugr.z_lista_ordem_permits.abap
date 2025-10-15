function z_lista_ordem_permits.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_USER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     REFERENCE(I_AUFNR) TYPE  AUFK-AUFNR OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_LINES) TYPE  NUMC2
*"  TABLES
*"      T_ORDENS STRUCTURE  ZPMR0003 OPTIONAL
*"----------------------------------------------------------------------

* Declaração dos tipos de dados globais
  types: begin of ty_zpmr0002.
           include  type zpmr0002.
  types:   iwerk   type aufk-werks,

           counter type n length 6,
         end of ty_zpmr0002.

* * Declaração dos tipos de dados globais
  types: begin of ty_zpmr0011.
           include  type zpmr0011.
  types:   iwerk   type aufk-werks,

           counter type n length 6,
         end of ty_zpmr0011.

** Tabelas
  data: gt_ihgns    type table of ihgns,
        gt_ordens   type table of zpmr0003,
        gt_zpmr0002 type table of ty_zpmr0002 with header line,
        gt_zpmr0011 type table of ty_zpmr0011 with header line.

  data  order_types type rsis_t_range.

  data: zt_zpmr0002 type table of zpmr0002 with header line.

** Work áreas
  data: gw_ordens     type zpmr0003,
        gw_ordens_aux type zpmr0003.

** Variáveis
  data: lv_seq_hist type n length 6,
        lv_seq_novo type n length 6,
        lv_nivel    type i,
        lv_objnr    type zpmr0003-objnr,
        lv_aufnr    type zpmr0003-aufnr.

  field-symbols: <fs_ordens> type zpmr0003,
                 <fs_ihgns>  type ihgns,
                 <fs_0002>   type ty_zpmr0002.

  select *
  from zpmr0002
  into table gt_zpmr0002
  where ( aprovador eq i_user or usua_subst eq i_user and data_lim >= sy-datum ).
  if sy-subrc ne 0.
    select *
      from zpmr0011
      into table gt_zpmr0011
      where ( aprovador eq i_user or usua_subst eq i_user and data_lim >= sy-datum ).
    if sy-subrc eq 0.
      move-corresponding gt_zpmr0011[] to gt_zpmr0002[].
    endif.
  endif.


  loop at gt_zpmr0002 assigning field-symbol(<wa_zpmr0002>).
    if <wa_zpmr0002>-usua_subst is not initial and <wa_zpmr0002>-data_lim >= sy-datum.
      <wa_zpmr0002>-aprovador = <wa_zpmr0002>-usua_subst.
    endif.
  endloop.

  check gt_zpmr0002[] is not initial.

  loop at gt_zpmr0002 assigning <fs_0002>.
    <fs_0002>-iwerk = <fs_0002>-centro_desp(4).

    select *
      from ztparam
      into table @data(_parameters)
     where param = 'TP_ORDEM'
       and const = @<fs_0002>-iwerk.

    loop at _parameters into data(_parameter).
      append value #( sign = 'I' option = 'EQ' low = _parameter-zval ) to order_types.
    endloop.

    move <fs_0002>-nivel to <fs_0002>-counter.
  endloop.

  sort order_types by low.
  delete adjacent duplicates from order_types comparing low.

  if i_aufnr is initial.
    select distinct a~mandt a~werks a~erdat h~equnr e~eqktx a~aufnr a~objnr a~ktext a~user4 a~erfzeit i~counter a~waers
      into corresponding fields of table gt_ordens
      from aufk as a
      inner join ihsg  as i on i~objnr eq a~objnr
      inner join afih  as h on h~aufnr eq a~aufnr
      left  join eqkt  as e on e~equnr eq h~equnr
      for all entries in gt_zpmr0002
      where ( a~phas0 eq 'X' and
              a~phas1 eq ' ' )
       and  a~auart in order_types
       and  a~werks eq gt_zpmr0002-iwerk
       and  i~lvorm eq ''.

  else.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = i_aufnr
      importing
        output = lv_aufnr.

    select distinct a~mandt a~werks a~erdat h~equnr e~eqktx a~aufnr a~objnr a~ktext a~user4 a~erfzeit i~counter a~waers
      into corresponding fields of table gt_ordens
      from aufk as a
      inner join ihsg  as i on i~objnr eq a~objnr
      inner join afih  as h on h~aufnr eq a~aufnr
      left  join eqkt  as e on e~equnr eq h~equnr
      where ( a~phas0 eq 'X' and
              a~phas1 eq ' ' )
*       AND  AUART   EQ 'ZPM7'
*       AND  WERKS   EQ GT_ZPMR0002-IWERK".
       and  a~aufnr eq lv_aufnr
       and  i~lvorm   eq ''.

  endif.

  check gt_ordens is not initial.
  sort gt_ordens by aufnr counter.

  select *
    from ihgns
    into table gt_ihgns
    for all entries in gt_ordens
    where objnr   eq gt_ordens-objnr
     and  counter eq gt_ordens-counter.

  sort gt_ihgns by objnr counter ascending gendatum descending gentime descending.

** Deixa apenas registro atuais(ativos)
  loop at gt_ihgns assigning <fs_ihgns>.
    delete gt_ihgns where gendatum le <fs_ihgns>-gendatum
                     and  counter  eq <fs_ihgns>-counter
                     and  gentime  ne <fs_ihgns>-gentime
                     and  objnr    eq <fs_ihgns>-objnr.

  endloop.

  unassign <fs_ihgns>.

  sort gt_ihgns by objnr   ascending
                   counter ascending
                   geniakt descending.

  loop at gt_ordens into gw_ordens.
    if lv_objnr ne gw_ordens-objnr.
      lv_objnr = gw_ordens-objnr.
      clear lv_seq_novo.
    endif.

    add 1 to lv_seq_novo.

    clear: gw_ordens_aux, lv_seq_hist.
    read table t_ordens into gw_ordens_aux with key objnr = gw_ordens-objnr.
    if sy-subrc is initial.
      continue.
    endif.

    "//Verifica histórico de aprovação
    select single *
      from zpmr0006
      into @data(_historico_wf)
     where aufnr  = @gw_ordens-aufnr
       and status = 'R'.

    if sy-subrc is initial.
      if ( _historico_wf-vlr_estimado eq gw_ordens-user4 ).
        gw_ordens-status = 'R'.
      else.
        gw_ordens-status = 'P'.
        delete from zpmr0006 where aufnr = gw_ordens-aufnr.
      endif.
    endif.

** Verificar permit sem histórico para ordem
    read table gt_ihgns assigning <fs_ihgns> with key objnr   = gw_ordens-objnr
                                                      counter = gw_ordens-counter.
*     IF SY-SUBRC = 0.
    if sy-subrc is not initial.
      read table t_ordens into gw_ordens_aux with key objnr = gw_ordens-objnr.
      if sy-subrc is not initial.
        move lv_seq_novo to gw_ordens-counter.
        append gw_ordens to t_ordens.
      endif.
      continue.
    endif.

** Verifica se há histórico de permit para a ordem
    loop at gt_ihgns assigning <fs_ihgns> where objnr   = gw_ordens-objnr
                                           and  counter = gw_ordens-counter.
      add 1 to lv_seq_hist.
      if <fs_ihgns>-geniakt eq 'X'.
        read table t_ordens into gw_ordens_aux with key objnr = gw_ordens-objnr.
        if sy-subrc is not initial.
          move lv_seq_hist to gw_ordens-counter.
          append gw_ordens to t_ordens.
        endif.
      endif.
    endloop.
  endloop.


** Remove permits de niveis diferentes do usuário entrado
  loop at t_ordens assigning <fs_ordens>.
    read table gt_zpmr0002 with key iwerk   = <fs_ordens>-werks
                                    counter = <fs_ordens>-counter.
    if sy-subrc is not initial.
      delete t_ordens where objnr = <fs_ordens>-objnr.
    endif.
  endloop.

  e_lines = lines( t_ordens ).
  clear order_types.

endfunction.
