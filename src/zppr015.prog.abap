**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Welgem Barbosa ( welgem.barbosa@amaggi.com.br )                      |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Cleudo Ferreira ( claudo.ferreira@amaggi.com.br )                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Report de Reprocessamento da Embalagens                                   |*
**/===========================================================================\*

report zppr015.

tables: zppt0015, mchb.

parameters bloco type zppt0015-bloco no-display.
select-options: charg for mchb-charg no-display.

data w_0011  type zppt0011.
data _mblnr type mblnr.
data _mjahr type mjahr.

data: zcheck_lote type char01,
      it_MCHA     type mcha_tty,
      zcharg      type charg_d.

clear: w_0011, _mblnr, _mjahr. "Ajuste referente o IR192540 / AOENNING

data(obj_create) = new zcl_pm_embalagens( ).

select *
  from zppt0019
    into table @data(t_0019)
    where status eq 'A'
      and acao eq 'entrada'.

if t_0019 is not initial.

* BEG - STEFANINI - FT - 2000016202 - IR192540 - PACK
  if bloco is not initial.
    delete t_0019 where bloco ne bloco.
  endif.

  if charg is not initial.
    delete t_0019 where lote_individual not in charg.
  endif.

  if t_0019 is not initial.

    select *
      from zppt0011
      into table @data(t_0011)
      for all entries in @t_0019
        where charg eq @t_0019-lote_individual.

    select *
      from mchb
      into table @data(t_mchb)
      for all entries in @t_0019
        where charg eq @t_0019-lote_individual.

    loop at t_0019 into data(w_0019).

      clear: w_0011.

      w_0019-lote_individual = |{ w_0019-lote_individual case = upper }|.
*    w_0019-codigo = |{ w_0019-codigo ALPHA = IN }|.
      call function 'CONVERSION_EXIT_MATN1_INPUT'
        exporting
          input        = w_0019-codigo
        importing
          output       = w_0019-codigo
        exceptions
          length_error = 1
          others       = 2.
      if sy-subrc <> 0.
* Implement suitable error handling here
      endif.

      w_0019-deposito_saida = |{ w_0019-deposito_saida case = upper }|.

      if not line_exists( t_0011[ charg = w_0019-lote_individual ] ).
        if line_exists( t_mchb[ charg = w_0019-lote_individual ] ).

          call method obj_create->set_doc_material
            exporting
              matnr = w_0019-codigo
              werks = w_0019-centro
              lgort = w_0019-deposito_saida
              charg = w_0019-lote_individual.

          call method obj_create->set_vencimento
            exporting
              matnr = w_0019-codigo
              charg = w_0019-lote_fabricante.

          call method obj_create->set_lote
            exporting
              lote = w_0019-lote_fabricante.

          call method obj_create->set_pedido
            exporting
              matnr = w_0019-codigo
              werks = w_0019-centro
              lgort = w_0019-deposito_saida
              charg = obj_create->get_lote( ).

          w_0011 = value #(
                                    ebeln = obj_create->get_pedido( )
                                    matnr = w_0019-codigo
                                    werks = w_0019-centro
                                    lgort = w_0019-deposito_saida
                                    charg = w_0019-lote_individual
                                    mblnr = obj_create->get_mblnr( )
                                    mjahr = obj_create->get_mjahr( )
                                    clabs = w_0019-quantidade
                                    vfdat = obj_create->get_vencimento( )
                                    lfabr = w_0019-lote_fabricante
                                    usnam = sy-uname
                                    data_atual = sy-datum
                                    hora_atual = sy-uzeit
                                    id_movimentacao = w_0019-id_movimentacao
                                ).

          modify zppt0011 from w_0011.
          commit work and wait.

          if sy-subrc is initial.
            update zppt0019 set status = 'F'
                            processado = 'P'
                  where lote_individual eq w_0019-lote_individual
                  and status eq 'A'
                  and acao eq 'entrada'.
            commit work and wait.
          endif.

        endif.
      else.
        update zppt0019 set status = 'F'
          where lote_individual eq w_0019-lote_individual
          and status eq 'A'
          and acao eq 'entrada'.
        commit work and wait.
      endif.

    endloop.

  endif.
endif.
* END - STEFANINI - FT - 2000016202 - IR192540 - PACK

free t_0019.

select *
  from zppt0015
  into table @data(it_0015)
  where lote_individual in @charg
    and bloco eq @bloco.

check it_0015 is not initial.

select *
  from zppt0019
    into table t_0019
  for all entries in it_0015
    where lote_individual eq it_0015-lote_individual
      and status eq 'A'
      and processado ne abap_true
      and acao in ( 'transferencia', 'fornecedor', 'terceiros' ).

if t_0019 is not initial.

  select *
    from mseg
    into table @data(it_mseg)
    for all entries in @t_0019
    where charg eq @t_0019-lote_individual
      and bwart eq '311'.

  sort it_mseg by charg.
  delete adjacent duplicates from it_mseg comparing charg.

  loop at it_mseg into data(wa_mseg).

    read table t_0019 into w_0019 with key lote_individual = wa_mseg-charg .
    if sy-subrc is initial.
      update zppt0011 set mblnr = wa_mseg-mblnr
                          mjahr = wa_mseg-mjahr
                          umlgo = wa_mseg-umlgo
                          umcha = wa_mseg-umcha
                          r_werks = w_0019-centro_receptor
                          id_movimentacao = w_0019-id_movimentacao
                          data_atual = sy-datum
                          hora_atual = sy-uzeit
      where charg = wa_mseg-charg
      and werks = wa_mseg-werks.

      update zppt0019 set processado = abap_true
      where lote_individual eq w_0019-lote_individual
         and empresa eq w_0019-empresa
         and centro eq w_0019-centro
         and acao eq w_0019-acao.

      commit work and wait.

    endif.

  endloop.
endif.


"INICIO USER STORY 152607 / AOENNING
free: t_0019, it_0015.

select *
  from zppt0015
  into table it_0015
  where lote_individual in charg
    and bloco eq bloco.

check it_0015 is not initial.

select *
  from zppt0019
    into table t_0019
  for all entries in it_0015
    where lote_individual eq it_0015-lote_individual
      and status eq 'A'
      and processado ne abap_true
      and acao in ( 'transferenciadeposito' ).

check t_0019 is not initial.

    free: it_mseg.
    select *
    from mseg
    into table it_mseg
    for all entries in t_0019
    where charg eq t_0019-lote_individual
      and bwart eq '309'.

sort it_mseg by charg.
delete adjacent duplicates from it_mseg comparing charg.

loop at it_mseg into wa_mseg.

  read table t_0019 into w_0019 with key lote_individual = wa_mseg-charg .
  if sy-subrc is initial.
    update zppt0011 set mblnr = wa_mseg-mblnr
                        mjahr = wa_mseg-mjahr
                        umlgo = wa_mseg-umlgo
                        umcha = wa_mseg-umcha
                        r_werks = w_0019-centro_receptor
                        id_movimentacao = w_0019-id_movimentacao
                        data_atual = sy-datum
                        hora_atual = sy-uzeit
    where charg = wa_mseg-charg
    and werks = wa_mseg-werks.


    update zppt0019 set processado = abap_true
                        status     = 'F'
    where lote_individual eq w_0019-lote_individual
       and empresa eq w_0019-empresa
       and centro eq w_0019-centro
       and acao eq w_0019-acao.

    commit work and wait.

  endif.

endloop.
"FIM USER STORY 152607 / AOENNING
