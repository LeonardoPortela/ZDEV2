*&---------------------------------------------------------------------*
*&  Include           ZLESR0123_FORM
*&---------------------------------------------------------------------*

initialization.

at selection-screen output.


  loop at screen.
    if p_route is not initial.
      if ( screen-name = 'P_ZONE1-LOW' or screen-name = 'P_ZONE2-LOW' ).
        screen-input = 0.
        modify screen.
      endif.
    elseif p_zone1 is not initial and
           p_zone2 is not initial.
      if screen-name = 'P_ROUTE-LOW'.
        screen-input = 0.
        modify screen.
      endif.

    elseif p_zone1 is not initial and
           p_zone2 is  initial.
      if screen-name = 'P_ROUTE-LOW'.
        screen-input = 0.
        modify screen.
      endif.
    elseif p_zone1 is initial and
           p_zone2 is not initial.
      if screen-name = 'P_ROUTE-LOW'.
        screen-input = 0.
        modify screen.
      endif.
    endif.
  endloop.


start-of-selection.

  perform selecao_dados.

end-of-selection.


form selecao_dados.

  perform busca_dados_a900.
  perform busca_dados_a911.
  perform busca_dados_a910.
  perform busca_dados_a915.
  perform busca_dados_a918.
  perform busca_dados_a919.
  perform busca_dados_a933.
  perform busca_dados_a934.
  perform busca_dados_a938.
  perform busca_dados_a939.
  perform busca_dados_a940.
  perform imprimi_alv.

endform.

form busca_dados_a900.
  "data

  if p_data is not initial and  p_shtyp is initial and
     p_lifnr is initial    and  p_route is initial and
     p_zone1 is initial    and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    "data e tipo de frete
  elseif  p_data is not initial  and  p_shtyp is not initial
     and  p_lifnr is initial     and  p_route is initial
     and  p_zone1 is initial     and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data e Agente de Frete e tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl eq 'F'
      and  kschl  eq 'ZFRE'
      and  tdlnr  in p_lifnr
      and  datbi  >= p_data-low
      and  datab  <= p_data-low
      and  shtyp  in p_shtyp.

    " data e Itinerário
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data e Itinerário e tipo de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data, agente de frete e itinerario
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data, agente de frete e itinerario e tipo de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
      from a900
      into table t_a900
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi  >= p_data-low
      and   datab  <= p_data-low
      and   shtyp  in p_shtyp.

    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi  >= p_data-low
        and   datab  <= p_data-low
        and   shtyp  in p_shtyp.
    endif.

    " data, agente de frete,  zona de partida e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi  >= p_data-low
        and   datab  <= p_data-low.
    endif.

    " data,  zona de partida
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is  initial      and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  = t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " data,  zona de partida E tipo de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  = t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " data,  e zona de chegada
  elseif p_data  is not initial and  p_shtyp is initial
    and  p_lifnr is initial     and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab  sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  = t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " data,  e zona de chegada E TIPO DE FRETE
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  = t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1. "zona de partida informada na tela

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " data, agente de frete, zona de partida E TIPO DE FRETE
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1. "zona de partida informada na tela

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " data, agente de frete,  e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
       where aland = 'BR'
        and  lland = 'BR'
        and  lzone in p_zone2. "zona de chegada informada na tela

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " data, agente de frete,  e zona de chegada e tipo de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2. "zona de chegada informada na tela

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.


    " data, zona de chegada e zona de partida
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
       where aland = 'BR'
        and  lland = 'BR'
        and  azone in p_zone1
        and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " data, zona de chegada , zona de partida E tipo de frete
  elseif p_data  is not initial    and p_shtyp is not initial
     and p_lifnr is initial        and p_route is initial
     and p_zone1 is not initial    and p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select knumh  shtyp  tdlnr  route  add01  kappl  kschl  datbi  datab sdabw
        from a900
        into table t_a900
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  route  eq t_trolz-route
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

  endif.

  if t_a900 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein krech
      from konp
      into table t_konp
      for all entries in t_a900
      where  knumh  eq t_a900-knumh
      and    kappl  eq  'F'
      and    kschl  eq 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A900'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    select route  bezei
        from tvrot
        into table t_tvrot
      for all entries in t_a900
        where route eq t_a900-route.

    select sdabw  bezei
       from tvsakt
       into table t_tvsakt
     for all entries in t_a900
       where sdabw eq t_a900-sdabw
       and   spras eq sy-langu.

    perform trata_dados_a900.
  endif.
endform.

form busca_dados_a911.

  "data
  if  p_data is not initial  and p_shtyp is initial
  and p_lifnr is initial     and p_route is initial
  and p_zone1 is initial     and p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    "data e tipo de frete
  elseif p_data   is not  initial  and p_shtyp  is not initial
     and p_lifnr  is initial       and p_route  is initial
     and p_zone1  is initial       and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data e Agente de Frete E TIPO DE FRETE
  elseif p_data is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Itinerário
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is not initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route   kappl  kschl  datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data, Itinerário e tipo de frete
  elseif p_data  is not initial and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route   kappl  kschl  datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
    " data, itinerario e agente de frete
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.
    " data, itinerario, TIPO DE FRETE e agente de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a911
      into table t_a911
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data, agente de frete, zona de partida e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  eq t_trolz-route
      and   datbi >= p_data-low
      and   datab <= p_data-low.
    endif.


    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp  in p_shtyp.
    endif.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, TIPO DE FRETE, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data, agente de frete, zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.
    if t_trolz[] is not initial.
      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.
    " data, tipo de frete, agente de frete, zona de chegada
  elseif p_data  is not initial and  p_shtyp is not initial
    and  p_lifnr is not initial and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.


    if t_trolz[] is not initial.
      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.
    if t_trolz[] is not initial.
      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, tipo de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.
    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
    " data,  zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.
    " data, TIPO DE FRETE, zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
    " data,  zona de chegada E ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.
    " data, TIPO DE FRETE, zona de chegada E ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
        from a911
        into table t_a911
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   route  eq t_trolz-route
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
  endif.

  if t_a911 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein krech
      from konp
      into table t_konp
      for all entries in t_a911
      where  knumh = t_a911-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A911'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    select route  bezei
    from tvrot
    into table t_tvrot
    for all entries in t_a911
    where route eq t_a911-route.

    select sdabw  bezei
    from tvsakt
    into table t_tvsakt
    for all entries in t_a911
    where sdabw eq t_a911-sdabw
    and   spras eq sy-langu.


    perform trata_dados_a911.

  endif.

endform.

form busca_dados_a910.

  "DATA
  if   p_data is not  initial  and  p_shtyp is initial
  and  p_lifnr is initial      and  p_route is initial
  and  p_zone1 is initial      and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
      from a910
      into table t_a910
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    "DATA E TIPO DE FRETE
  elseif p_data is not  initial
    and  p_shtyp is not initial
    and  p_lifnr is initial
    and  p_route is initial
    and  p_zone1 is initial
    and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
      from a910
      into table t_a910
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data e agente de frete
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
      from a910
      into table t_a910
      where kappl = 'F'
      and   kschl = 'ZFRE'
      and   tdlnr in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data , TIPO DE FRETE E agente de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
      from a910
      into table t_a910
      where kappl = 'F'
      and   kschl = 'ZFRE'
      and   tdlnr in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data e itinerario
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is initial        and  p_route is not initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, TIPO DE FRETE e itinerario
  elseif p_data  is not initial
    and  p_shtyp is not initial
    and  p_lifnr is initial
    and  p_route is not initial
    and  p_zone1 is initial
    and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
    " DATA, AGENTE DE FRETE E ITINERARIO
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is not initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.


    " DATA, TIPO DE FRETE ,AGENTE DE FRETE E ITINERARIO
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp  in p_shtyp.
    endif.
    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.
    " DATA, AGENTE DE FRETE, TIPO DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.


    " DATA, AGENTE DE FRETE, tipo de frete, ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE CHEGADA
  elseif p_data  is not initial and  p_lifnr is not initial
    and  p_route is initial     and  p_zone1 is initial
    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, AGENTE DE FRETE,   TIPO DE FRETE E ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA,  ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is  initial     and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, tipo de frete e ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA,  ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE E   ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA,  ZONA DE CHEGADA  E ZONA DE PARTIDA
  elseif p_data  is not initial
    and  p_lifnr is  initial
    and  p_route is initial
    and  p_zone1 is not initial
    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE, ZONA DE CHEGADA  E ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez
        from a910
        into table t_a910
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
  endif.

  if t_a910 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a910
      where  knumh = t_a910-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A910'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.
    perform trata_dados_a910.

  endif.
endform.

form busca_dados_a915.

  " DATA
  if   p_data is not  initial   and  p_shtyp is initial
  and  p_lifnr is initial       and  p_route is initial
  and  p_zone1 is initial       and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
      from a915
      into table t_a915
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " DATA E TIPO DE FRETE
  elseif p_data is not  initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
      from a915
      into table t_a915
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " DATA E AGENTE DE FRETE
  elseif p_data is not initial
    and  p_shtyp is initial
    and  p_lifnr is not initial
    and  p_route is initial
    and  p_zone1 is initial
    and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
      from a915
      into table t_a915
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " DATA , TIPO DE FRETE E AGENTE DE FRETE
  elseif p_data is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
      from a915
      into table t_a915
      where kappl = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " DATA E ITINERARIO
  elseif p_data is not initial  and  p_shtyp is initial
    and  p_lifnr is initial     and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA , TIPO DE FRETE E ITINERARIO
  elseif p_data is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.


    " DATA, AGENTE DE FRETE E ITINERARIO
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE , AGENTE DE FRETE E ITINERARIO
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, AGENTE DE FRETE, TIPO DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, AGENTE DE FRETE, TIPO DE FRETE , ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is not initial.
    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, AGENTE DE FRETE, tipo de frete e  ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, ZONA DE PARTIDA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE E ZONA DE PARTIDA
  elseif p_data  is not initial and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is initial
    and  p_zone1 is not initial and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA,  ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is  initial     and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE E ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, ZONA DE PARTIDA, ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE, ZONA DE PARTIDA, ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select  kappl  kschl  knumh  shtyp  tdlnr  lzonea  lzonez add01
        from a915
        into table t_a915
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
  endif.

  if t_a915 is not initial.
    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a915
      where  knumh = t_a915-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A915'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    perform trata_dados_a915.
  endif.
endform.

form busca_dados_a918.

  " DATA
  if   p_data is not initial  and  p_shtyp is initial
  and  p_lifnr is initial     and  p_route is initial
  and  p_zone1 is initial     and  p_zone2 is initial.

    select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
      from a918
      into table t_a918
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  datbi >= p_data-low
      and  datab <= p_data-low.

    " DATA E TIPO DE FRETE
  elseif p_data is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
      from a918
      into table t_a918
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  datbi >= p_data-low
      and  datab <= p_data-low
      and  shtyp in p_shtyp.

    " DATA E AGENTE DE FRETE
  elseif p_data is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
      from a918
      into table t_a918
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  tdlnr in p_lifnr
      and  datbi >= p_data-low
      and  datab <= p_data-low.

    " DATA, TIPO DE FRETE E AGENTE DE FRETE
  elseif p_data is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
      from a918
      into table t_a918
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  tdlnr in p_lifnr
      and  datbi >= p_data-low
      and  datab <= p_data-low
      and  shtyp in p_shtyp.

    " DATA E ITINERARIO
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA,  TIPO DE FRETE  E ITINERARIO
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    "data , agente de frete e itinerario
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    "data , TIPO DE FRETE, agente de frete e itinerario
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data, agente de frete, zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, agente de frete, TIPO DE FRETE, zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
    " data, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, agente de frete, tipo de frete e zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data, agente de frete, zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
    where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, agente de frete, TIPO DE FRETE E zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
    where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data E  zona de partida
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is  initial.

    select  aland lland azone lzone route
       from trolz
       into table t_trolz
    where aland = 'BR'
     and  lland = 'BR'
     and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, TIPO DE FRETE E  zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

    select  aland lland azone lzone route
       from trolz
       into table t_trolz
    where aland = 'BR'
     and  lland = 'BR'
     and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data E zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
    where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, TIPO DE FRETE E zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
    where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonez eq t_trolz-lzone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " data E zona de chegada E PARTIDA
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is initial        and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " data, TIPO DE FRETE , zona de chegada E PARTIDA
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is initial        and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl knumh  shtyp  tdlnr  lzonea lzonez add01 matnr
        from a918
        into table t_a918
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   lzonez eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.
  endif.

  if t_a918 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a918
      where  knumh = t_a918-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A918'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    perform trata_dados_a918.
  endif.
endform.

form busca_dados_a919.
  "DATA
  if  p_data is not  initial and p_shtyp is initial
  and p_lifnr is initial     and p_route is initial
  and p_zone1 is initial     and p_zone2 is initial.

    select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
      from a919
      into table t_a919
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  datbi >= p_data-low
      and  datab <= p_data-low.

    "DATA E TIPO DE FRETE
  elseif p_data is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
      from a919
      into table t_a919
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  datbi >= p_data-low
      and  datab <= p_data-low
      and  shtyp in p_shtyp.

    " DATA E AGENTE DE FRETE
  elseif p_data is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
      from a919
      into table t_a919
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  tdlnr in p_lifnr
      and  datbi >= p_data-low
      and  datab <= p_data-low.

    " DATA, TIPO DE FRETE E AGENTE DE FRETE
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
      from a919
      into table t_a919
      where kappl = 'F'
      and  kschl  = 'ZFRE'
      and  tdlnr in p_lifnr
      and  datbi >= p_data-low
      and  datab <= p_data-low
      and  shtyp in p_shtyp.

    " DATA E ITINERARIO
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is initial         and  p_route is not initial
    and  p_zone1 is initial         and p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " DATA , TIPO DE FRETE E ITINERARIO
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is initial        and  p_route is not initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.


    " DATA, AGENTE FRETE E ITINERARIO
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is not initial
    and  p_zone1 is initial         and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.


    " DATA, TIPO DE FRETE , AGENTE FRETE E ITINERARIO
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is not initial
    and  p_zone1 is initial        and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  route in p_route.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.
    " DATA, TIPO DE FRETE, AGENTE DE FRETE, ZONA DE PARTIDA E ZONA DE CHEGADA
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE PARTIDA
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.


    " DATA, TIPO DE FRETE , AGENTE DE FRETE, ZONA DE PARTIDA
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr in p_lifnr
        and  lzonea eq t_trolz-azone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " DATA, AGENTE DE FRETE, ZONA DE CHEGADA
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " DATA, AGENTE DE FRETE, TIPO DE FRETE E ZONA DE CHEGADA
  elseif p_data  is not initial     and  p_shtyp is not initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is initial         and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " DATA, ZONA DE CHEGADA E PARTIDA
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is initial        and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " DATA, ZONA DE CHEGADA, TIPO DE FRETE  E PARTIDA
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1
    and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonea eq t_trolz-azone
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.

    " DATA, E ZONA DE CHEGADA
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is initial        and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE E ZONA DE CHEGADA
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is initial        and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl  = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea eq t_trolz-azone
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
    endif.

    " DATA, ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low.
    endif.

    " DATA, TIPO DE FRETE E ZONA DE CHEGADA
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
   where aland = 'BR'
    and  lland = 'BR'
    and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select kappl  kschl  knumh   shtyp   tdlnr   matnr   lzonea  lzonez
        from a919
        into table t_a919
        for all entries in t_trolz
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  lzonez eq t_trolz-lzone
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
    endif.
  endif.

  if t_a919 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a919
      where  knumh = t_a919-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A919'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    perform trata_dados_a919.
  endif.

endform.

form trata_dados_a900.
  loop at t_a900 into w_a900.

    read table t_trolz into w_trolz with key route = w_a900-route.
    if sy-subrc = 0.
      w_saida-lzonea = w_trolz-azone.
      w_saida-lzonez = w_trolz-lzone.

      select land1  zone1  vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-azone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-azone.
      if sy-subrc = 0.
        w_saida-vtexta = w_tzont-vtext.
      endif.

      clear : t_tzont, w_tzont.

      select land1 zone1  vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-lzone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-lzone.
      if sy-subrc = 0.
        w_saida-vtextz = w_tzont-vtext.
      endif.
    endif.

    w_saida-shtyp  =  w_a900-shtyp.
    w_saida-tdlnr  =  w_a900-tdlnr.
    w_saida-route  =  w_a900-route.
    w_saida-ddtext = w_dd02t-ddtext.

    if w_a900-add01 = '0000000001'.
      w_saida-add01  = 'Sim'.
    elseif w_a900-add01 = '0000000002'.
      w_saida-add01  = 'Não'.
    endif.


    read table t_konp into w_konp with key knumh = w_a900-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
      if w_konp-krech = 'A' .
        w_saida-kbetr = w_saida-kbetr / 10.
      endif.
    endif.

    read table t_tvrot into w_tvrot with key route = w_a900-route.
    if sy-subrc = 0.
      w_saida-bezei =  w_tvrot-bezei.
    endif.

    w_saida-sdabw = w_a900-sdabw.
    read table t_tvsakt into data(w_tvsakt) with key sdabw = w_a900-sdabw.
    if sy-subrc = 0.
      w_saida-bezec =  w_tvsakt-bezei.
    endif.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a900, w_konp, w_tzont.
  endloop.

endform.

form trata_dados_a911.
  loop at t_a911 into w_a911.

    read table t_trolz into w_trolz with key route = w_a911-route.
    if sy-subrc = 0.
      w_saida-lzonea = w_trolz-azone.
      w_saida-lzonez = w_trolz-lzone.

      select land1 zone1 vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-azone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-azone.
      if sy-subrc = 0.
        w_saida-vtexta = w_tzont-vtext.
      endif.

      clear : t_tzont,  w_tzont.

      select land1 zone1  vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-lzone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-lzone.
      if sy-subrc = 0.
        w_saida-vtextz = w_tzont-vtext.
      endif.
    endif.

    read table t_tvrot into w_tvrot with key route = w_a911-route.
    if sy-subrc = 0.
      w_saida-bezei =  w_tvrot-bezei.
    endif.

    w_saida-sdabw = w_a911-sdabw.
    read table t_tvsakt into data(w_tvsakt) with key sdabw = w_a911-sdabw.
    if sy-subrc = 0.
      w_saida-bezec =  w_tvsakt-bezei.
    endif.

    w_saida-shtyp =  w_a911-shtyp.
    w_saida-tdlnr =  w_a911-tdlnr.
    w_saida-route =  w_a911-route.
    w_saida-ddtext = w_dd02t-ddtext.

    read table t_konp into w_konp with key knumh = w_a911-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
      if w_konp-krech = 'A' .
        w_saida-kbetr = w_saida-kbetr / 10.
      endif.
    endif.
    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a911, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a910.

  loop at t_a910 into w_a910.
    w_saida-shtyp  =  w_a910-shtyp.
    w_saida-tdlnr  =  w_a910-tdlnr.
    w_saida-lzonea =  w_a910-lzonea.
    w_saida-lzonez =  w_a910-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

    read table t_konp into w_konp with key knumh = w_a910-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.


    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a910-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a910-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont, w_tzont.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a910-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a910-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a910, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a915.

  loop at t_a915 into w_a915.
    w_saida-shtyp  =  w_a915-shtyp.
    w_saida-tdlnr  =  w_a915-tdlnr.
    w_saida-lzonea =  w_a915-lzonea.
    w_saida-lzonez =  w_a915-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

    if w_a915-add01 = '0000000001'.
      w_saida-add01  = 'Sim'.
    elseif w_a915-add01 = '0000000002'.
      w_saida-add01  = 'Não'.
    endif.

    read table t_konp into w_konp with key knumh = w_a915-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a915-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a915-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,
            w_tzont.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a915-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a915-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a915, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a918.

  loop at t_a918 into w_a918.
    w_saida-shtyp  =  w_a918-shtyp.
    w_saida-tdlnr  =  w_a918-tdlnr.
    w_saida-lzonea =  w_a918-lzonea.
    w_saida-lzonez =  w_a918-lzonez.
    w_saida-matnr  =  w_a918-matnr.
    w_saida-ddtext =  w_dd02t-ddtext.

    if w_a918-add01 = '0000000001'.
      w_saida-add01  = 'Sim'.
    elseif w_a918-add01 = '0000000002'.
      w_saida-add01  = 'Não'.
    endif.

    read table t_konp into w_konp with key knumh = w_a918-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1 zone1  vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a918-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a918-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,  w_tzont.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a918-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a918-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a918, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a919.

  loop at t_a919 into w_a919.
    w_saida-shtyp  =  w_a919-shtyp.
    w_saida-tdlnr  =  w_a919-tdlnr.
    w_saida-lzonea =  w_a919-lzonea.
    w_saida-lzonez =  w_a919-lzonez.
    w_saida-matnr  =  w_a919-matnr.
    w_saida-ddtext =  w_dd02t-ddtext.

    read table t_konp into w_konp with key knumh = w_a919-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a919-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a919-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont, w_tzont.

    select land1 zone1 vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a919-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a918-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a919, w_konp, w_tzont.

  endloop.

endform.



form imprimi_alv.

  clear: it_fieldcatalog[].
  perform preenche_cat using:
       'SHTYP'           'Tp. Transporte'                         '14'     ''      '' '' '',
       'TDLNR'           'Agente Frete'                           '15'     ''      '' '' '',
       'ROUTE'           'Itinerário'                             '10'     ''      '' '' '',
       'BEZEI'           'Descr. Itinerário'                      '40'     ''      '' '' '',
       'LZONEA'          'Zona Partida'                           '12'     ''      '' '' '',
       'VTEXTA'          'Descr. Zona Partida'                    '20'     ''      '' '' '',
       'LZONEZ'          'Zona Chegada'                           '12'     ''      '' '' '',
       'VTEXTZ'          'Descr. Zona Chegada'                    '20'     ''      '' '' '',
       'ADD01'           'Agregada'                               '10'     ''      '' '' '',
       'MATNR'           'Material'                               '20'     ''      '' '' '',
       'VSTEL'           'Local Neg.'                             '10'     ''      '' '' '',
       'KBETR'           'Valor'                                  '10'     ''      '' '' '',
       'PSTLZA'          'Cód.post.part.'                         '15'     ''      '' '' '',
       'VEGR5'           'Grp.UCs 5'                              '15'     ''      '' '' '',
       'KONWA'           'Unid. Cod'                              ' 9'     ''      '' '' '',
       'KMEIN'           'Unid. Medida'                           '10'     ''      '' '' '',
       'DDTEXT'          'Obs.'                                   '60'     ''      '' '' '',
       'SDABW'           'Cód. Proc.Esp.'                         '05'     ''      '' '' '',
       'BEZEC'           'Descr. Cód. Proc. Esp.'                 '30'     ''      '' '' ''.


  call screen 0100.

endform.

form preenche_cat using value(p_campo)
                        value(p_desc)
                        value(p_tam)
                        value(p_zero)
                        value(p_hot)
                        value(p_sum)
                        value(p_just).

  wa_fieldcatalog-fieldname   = p_campo.
  wa_fieldcatalog-coltext     = p_desc.
  wa_fieldcatalog-scrtext_l   = p_desc.
  wa_fieldcatalog-scrtext_m   = p_desc.
  wa_fieldcatalog-scrtext_s   = p_desc.


  wa_fieldcatalog-outputlen   = p_tam.
  wa_fieldcatalog-hotspot     = p_hot.
  wa_fieldcatalog-no_zero     = p_zero.
  wa_fieldcatalog-do_sum      = p_sum.
  wa_fieldcatalog-just        = p_just.

  append wa_fieldcatalog to it_fieldcatalog.


endform.
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_A933
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados_a933 .

  if  p_data is not initial   and  p_shtyp is initial  and
      p_lifnr is initial      and  p_route is initial  and
      p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    "data e tipo de frete
  elseif  p_data is not initial   and  p_shtyp is not initial
     and  p_lifnr is initial      and  p_route is initial
     and  p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data e Agente de Frete e tipo de frete
  elseif p_data is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   shtyp  in p_shtyp
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
        for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   tdlnr  in p_lifnr
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi  >= p_data-low
          and   datab  <= p_data-low
          and   shtyp  in p_shtyp.
    endif.

    " data, agente de frete,  zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.

      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   tdlnr  in p_lifnr
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi  >= p_data-low
          and   datab  <= p_data-low.
    endif.

    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is  initial     and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.

      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low.

    endif.

    " data,  zona de partida E tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1.

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low
          and   shtyp in p_shtyp.
    endif.

    " data,  e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low.
    endif.

    " data,  e zona de chegada E TIPO DE FRETE
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl eq 'F'
          and  kschl  eq 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and  datbi >= p_data-low
          and  datab <= p_data-low
          and  shtyp in p_shtyp.
    endif.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1. "zona de partida informada na tela

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl eq 'F'
          and   kschl  eq 'ZFRE'
          and   tdlnr  in p_lifnr
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low.
    endif.

    " data, agente de frete, zona de partida E TIPO DE FRETE
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  azone in p_zone1. "zona de partida informada na tela

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl eq 'F'
          and   kschl  eq 'ZFRE'
          and   tdlnr  in p_lifnr
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low
          and   shtyp in p_shtyp.
    endif.

    " data, agente de frete,  e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
       where aland = 'BR'
        and  lland = 'BR'
        and  lzone in p_zone2. "zona de chegada informada na tela

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
        from a933
        into table t_a933
          for all entries in t_trolz
          where kappl  eq 'F'
          and   kschl  eq 'ZFRE'
          and   tdlnr  in p_lifnr
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low.
    endif.

    " data, agente de frete,  e zona de chegada e tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland eq 'BR'
      and  lland eq 'BR'
      and  lzone in p_zone2. "zona de chegada informada na tela

    if t_trolz[] is not initial.

      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
          for all entries in t_trolz
          where kappl = 'F'
          and  kschl  = 'ZFRE'
          and  tdlnr  in p_lifnr
          and  lzonea eq t_trolz-azone
          and  lzonez eq t_trolz-lzone
          and  datbi >= p_data-low
          and  datab <= p_data-low
          and  shtyp in p_shtyp.
    endif.

    " data, zona de chegada e zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is initial         and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is not initial.

    select  aland lland azone lzone route
        from trolz
        into table t_trolz
       where aland eq 'BR'
        and  lland eq 'BR'
        and  azone in p_zone1
        and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
          for all entries in t_trolz
          where kappl eq 'F'
          and   kschl  eq 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low.
    endif.

    " data, zona de chegada , zona de partida E tipo de frete
  elseif p_data  is not initial     and p_shtyp is not initial
     and p_lifnr is initial         and p_route is initial
     and p_zone1 is not initial     and p_zone2 is not initial.

    select  aland lland azone lzone route
      from trolz
      into table t_trolz
     where aland = 'BR'
      and  lland = 'BR'
      and  azone in p_zone1
      and  lzone in p_zone2.

    if t_trolz[] is not initial.
      select kappl kschl datbi datab knumh tdlnr lzonea lzonez vstel shtyp
      from a933
      into table t_a933
          for all entries in t_trolz
          where kappl = 'F'
          and   kschl  = 'ZFRE'
          and   lzonea eq t_trolz-azone
          and   lzonez eq t_trolz-lzone
          and   datbi >= p_data-low
          and   datab <= p_data-low
          and   shtyp in p_shtyp.
    endif.

  endif.

  if t_a933 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a933
      where  knumh  eq t_a933-knumh
      and    kappl  eq 'F'
      and    kschl  eq 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A933'.

    select single tabname ddlanguage ddtext
      from dd02t    into w_dd02t
      where tabname eq tab
      and   ddlanguage eq 'PT'.

    clear tab.

*    SELECT ROUTE  BEZEI
*        FROM TVROT    INTO TABLE T_TVROT
*      FOR ALL ENTRIES IN T_A933
*        WHERE ROUTE EQ T_A933-ROUTE.

    perform trata_dados_a933.
  endif.
endform.

form trata_dados_a933.

  loop at t_a933 into w_a933.
    w_saida-shtyp  =  w_a933-shtyp.
    w_saida-tdlnr  =  w_a933-tdlnr.
    w_saida-lzonea =  w_a933-lzonea.
    w_saida-lzonez =  w_a933-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

*    IF W_A933-ADD01 = '0000000001'.
*      W_SAIDA-ADD01  = 'Sim'.
*    ELSEIF W_A933-ADD01 = '0000000002'.
*      W_SAIDA-ADD01  = 'Não'.
*    ENDIF.

    read table t_konp into w_konp with key knumh = w_a933-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a933-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a933-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,
            w_tzont.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a933-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a933-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    w_saida-vstel = w_a933-vstel.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a933, w_konp, w_tzont.

  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_A934
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados_a934 .

  if  p_data is not initial   and  p_shtyp is initial  and
      p_lifnr is initial      and  p_route is initial  and
      p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    "data e tipo de frete
  elseif  p_data is not initial   and  p_shtyp is not initial
     and  p_lifnr is initial      and  p_route is initial
     and  p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data e Agente de Frete e tipo de frete
  elseif p_data is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   shtyp  in p_shtyp
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*        FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low
        and   shtyp  in p_shtyp.
*    ENDIF.

    " data, agente de frete,  zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low.
*    ENDIF.

    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is  initial     and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.

*    ENDIF.

    " data,  zona de partida E tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data,  e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data,  e zona de chegada E TIPO DE FRETE
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and  kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete, zona de partida E TIPO DE FRETE
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete,  e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND = 'BR'
*        AND  LLAND = 'BR'
*        AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
      from a934
      into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete,  e zona de chegada e tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.
*
*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
    from a934
    into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, zona de chegada e zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is initial         and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND EQ 'BR'
*        AND  LLAND EQ 'BR'
*        AND  AZONE IN P_ZONE1
*        AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
    from a934
    into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, zona de chegada , zona de partida E tipo de frete
  elseif p_data  is not initial     and p_shtyp is not initial
     and p_lifnr is initial         and p_route is initial
     and p_zone1 is not initial     and p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp
    from a934
    into table t_a934
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

  endif.

  if t_a934 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a934
      where  knumh  eq t_a934-knumh
      and    kappl  eq 'F'
      and    kschl  eq 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A934'.

    select single tabname ddlanguage ddtext
      from dd02t    into w_dd02t
      where tabname eq tab
      and   ddlanguage eq 'PT'.

    clear tab.

*    SELECT ROUTE  BEZEI
*        FROM TVROT    INTO TABLE T_TVROT
*      FOR ALL ENTRIES IN T_A934
*        WHERE ROUTE EQ T_A934-ROUTE.

    perform trata_dados_a934.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_A938
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados_a938 .

  if  p_data is not initial   and  p_shtyp is initial  and
      p_lifnr is initial      and  p_route is initial  and
      p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    "data e tipo de frete
  elseif  p_data is not initial   and  p_shtyp is not initial
     and  p_lifnr is initial      and  p_route is initial
     and  p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data e Agente de Frete e tipo de frete
  elseif p_data is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   shtyp  in p_shtyp
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*        FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low
        and   shtyp  in p_shtyp.
*    ENDIF.

    " data, agente de frete,  zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low.
*    ENDIF.

    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is  initial     and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.

*    ENDIF.

    " data,  zona de partida E tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data,  e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data,  e zona de chegada E TIPO DE FRETE
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and  kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete, zona de partida E TIPO DE FRETE
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete,  e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND = 'BR'
*        AND  LLAND = 'BR'
*        AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
      from a938
      into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete,  e zona de chegada e tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
    from a938
    into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, zona de chegada e zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is initial         and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND EQ 'BR'
*        AND  LLAND EQ 'BR'
*        AND  AZONE IN P_ZONE1
*        AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
    from a938
    into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, zona de chegada , zona de partida E tipo de frete
  elseif p_data  is not initial     and p_shtyp is not initial
     and p_lifnr is initial         and p_route is initial
     and p_zone1 is not initial     and p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez pstlza shtyp matnr
    from a938
    into table t_a938
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

  endif.

  if t_a938 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a938
      where  knumh  eq t_a938-knumh
      and    kappl  eq 'F'
      and    kschl  eq 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A938'.

    select single tabname ddlanguage ddtext
      from dd02t    into w_dd02t
      where tabname eq tab
      and   ddlanguage eq 'PT'.

    clear tab.

*    SELECT ROUTE  BEZEI
*        FROM TVROT    INTO TABLE T_TVROT
*      FOR ALL ENTRIES IN T_A938
*        WHERE ROUTE EQ T_A938-ROUTE.

    perform trata_dados_a938.
  endif.
endform.

*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS_A939
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados_a939 .

  if  p_data is not initial   and  p_shtyp is initial  and
      p_lifnr is initial      and  p_route is initial  and
      p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    "data e tipo de frete
  elseif  p_data is not initial   and  p_shtyp is not initial
     and  p_lifnr is initial      and  p_route is initial
     and  p_zone1 is initial      and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi  >= p_data-low
      and   datab  <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data e Agente de Frete e tipo de frete
  elseif p_data is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   shtyp  in p_shtyp
      and   datbi  >= p_data-low
      and   datab  <= p_data-low.

    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*        FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low
        and   shtyp  in p_shtyp.
*    ENDIF.

    " data, agente de frete,  zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi  >= p_data-low
        and   datab  <= p_data-low.
*    ENDIF.

    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is  initial     and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.

*    ENDIF.

    " data,  zona de partida E tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data,  e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data,  e zona de chegada E TIPO DE FRETE
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and  kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is not initial     and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete, zona de partida E TIPO DE FRETE
  elseif p_data  is not initial    and  p_shtyp is not initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is not initial    and  p_zone2 is  initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  AZONE IN P_ZONE1. "zona de partida informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete,  e zona de chegada
  elseif p_data  is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial    and  p_route is initial
    and  p_zone1 is initial        and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND = 'BR'
*        AND  LLAND = 'BR'
*        AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
      from a939
      into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl  eq 'F'
        and   kschl  eq 'ZFRE'
        and   tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, agente de frete,  e zona de chegada e tipo de frete
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND EQ 'BR'
*      AND  LLAND EQ 'BR'
*      AND  LZONE IN P_ZONE2. "zona de chegada informada na tela
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
    from a939
    into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and  kschl  = 'ZFRE'
        and  tdlnr  in p_lifnr
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and  datbi >= p_data-low
        and  datab <= p_data-low
        and  shtyp in p_shtyp.
*    ENDIF.

    " data, zona de chegada e zona de partida
  elseif p_data  is not initial     and  p_shtyp is initial
    and  p_lifnr is initial         and  p_route is initial
    and  p_zone1 is not initial     and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*        FROM TROLZ
*        INTO TABLE T_TROLZ
*       WHERE ALAND EQ 'BR'
*        AND  LLAND EQ 'BR'
*        AND  AZONE IN P_ZONE1
*        AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
    from a939
    into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl eq 'F'
        and   kschl  eq 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low.
*    ENDIF.

    " data, zona de chegada , zona de partida E tipo de frete
  elseif p_data  is not initial     and p_shtyp is not initial
     and p_lifnr is initial         and p_route is initial
     and p_zone1 is not initial     and p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select kappl kschl datbi datab knumh tdlnr lzonea lzonez vegr5 shtyp
    from a939
    into table t_a939
*          FOR ALL ENTRIES IN T_TROLZ
        where kappl = 'F'
        and   kschl  = 'ZFRE'
        and   lzonea in p_zone1"T_TROLZ-AZONE
        and   lzonez in p_zone2"T_TROLZ-LZONE
        and   datbi >= p_data-low
        and   datab <= p_data-low
        and   shtyp in p_shtyp.
*    ENDIF.

  endif.

  if t_a939 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein
      from konp
      into table t_konp
      for all entries in t_a939
      where  knumh  eq t_a939-knumh
      and    kappl  eq 'F'
      and    kschl  eq 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A939'.

    select single tabname ddlanguage ddtext
      from dd02t    into w_dd02t
      where tabname eq tab
      and   ddlanguage eq 'PT'.

    clear tab.

*    SELECT ROUTE  BEZEI
*        FROM TVROT    INTO TABLE T_TVROT
*      FOR ALL ENTRIES IN T_A939
*        WHERE ROUTE EQ T_A939-ROUTE.

    perform trata_dados_a939.
  endif.
endform.

form busca_dados_a940.

  "data
  if  p_data is not initial  and p_shtyp is initial
  and p_lifnr is initial     and p_route is initial
  and p_zone1 is initial     and p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    "data e tipo de frete
  elseif p_data   is not  initial  and p_shtyp  is not initial
     and p_lifnr  is initial       and p_route  is initial
     and p_zone1  is initial       and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data e Agente de Frete
  elseif p_data is not initial    and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data e Agente de Frete E TIPO DE FRETE
  elseif p_data is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp  in p_shtyp.

    " data e Itinerário
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is not initial
    and  p_zone1 is initial       and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route   kappl  kschl  datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.

    " data, Itinerário e tipo de frete
  elseif p_data  is not initial and  p_shtyp is not initial
    and  p_lifnr is initial     and  p_route is not initial
    and  p_zone1 is initial     and  p_zone2 is initial.

    select knumh  shtyp  tdlnr  route   kappl  kschl  datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
    " data, itinerario e agente de frete
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low.
    " data, itinerario, TIPO DE FRETE e agente de frete
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is not initial
    and  p_zone1 is initial      and  p_zone2 is initial.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
      where kappl  eq 'F'
      and   kschl  eq 'ZFRE'
      and   tdlnr  in p_lifnr
      and   route  in p_route
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.

    " data, agente de frete, zona de partida e zona de chegada
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is not initial   and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
    where kappl  eq 'F'
    and   kschl  eq 'ZFRE'
    and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
    and   datbi >= p_data-low
    and   datab <= p_data-low.
*    ENDIF.


    " data, agente de frete, tipo de frete, zona de partida e zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp  in p_shtyp.
*  ENDIF.

    " data, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*      FOR ALL ENTRIES IN t_trolz
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low.
*    ENDIF.

    " data, TIPO DE FRETE, agente de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is not initial and  p_zone2 is initial.
*
*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
*    ENDIF.

    " data, agente de frete, zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is not initial  and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  LZONE IN P_ZONE2.
*    IF T_TROLZ[] IS NOT INITIAL.
    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low.
*    ENDIF.
    " data, tipo de frete, agente de frete, zona de chegada
  elseif p_data  is not initial and  p_shtyp is not initial
    and  p_lifnr is not initial and  p_route is initial
    and  p_zone1 is initial     and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  LZONE IN P_ZONE2.
*
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
      and   tdlnr  in p_lifnr
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
*  ENDIF.
    " data,  zona de partida
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1.
*    IF T_TROLZ[] IS NOT INITIAL.
    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*      FOR ALL ENTRIES IN t_trolz
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low.
*    ENDIF.

    " data, tipo de frete, zona de partida
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1.
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
*    ENDIF.
    " data,  zona de chegada
  elseif p_data  is not initial  and  p_shtyp is initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low.
*    ENDIF.
    " data, TIPO DE FRETE, zona de chegada
  elseif p_data  is not initial  and  p_shtyp is not initial
    and  p_lifnr is initial      and  p_route is initial
    and  p_zone1 is initial      and  p_zone2 is not initial.
*
*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.

    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
*    ENDIF.
    " data,  zona de chegada E ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial  and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*      FOR ALL ENTRIES IN t_trolz
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low.
*    ENDIF.
    " data, TIPO DE FRETE, zona de chegada E ZONA DE PARTIDA
  elseif p_data  is not initial   and  p_shtyp is not initial
    and  p_lifnr is initial       and  p_route is initial
    and  p_zone1 is not initial   and  p_zone2 is not initial.

*    SELECT  ALAND LLAND AZONE LZONE ROUTE
*      FROM TROLZ
*      INTO TABLE T_TROLZ
*     WHERE ALAND = 'BR'
*      AND  LLAND = 'BR'
*      AND  AZONE IN P_ZONE1
*      AND  LZONE IN P_ZONE2.
*
*    IF T_TROLZ[] IS NOT INITIAL.
    select  knumh  shtyp  tdlnr  route  kappl  kschl datbi  datab sdabw
      from a940
      into table t_a940
*        FOR ALL ENTRIES IN T_TROLZ
      where kappl  = 'F'
      and   kschl  = 'ZFRE'
*        AND   ROUTE  EQ T_TROLZ-ROUTE
      and   datbi >= p_data-low
      and   datab <= p_data-low
      and   shtyp in p_shtyp.
*    ENDIF.
  endif.

  if t_a940 is not initial.

    select  knumh  kappl  kschl  kbetr  konwa  kmein krech
      from konp
      into table t_konp
      for all entries in t_a940
      where  knumh = t_a940-knumh
      and    kappl  =  'F'
      and    kschl  = 'ZFRE'
      and    loevm_ko <> 'X'.

    tab = 'A940'.

    select single tabname ddlanguage ddtext
      from dd02t
      into w_dd02t
      where tabname = tab
      and   ddlanguage = 'PT'.

    clear tab.

    select route  bezei
    from tvrot
    into table t_tvrot
    for all entries in t_a940
    where route eq t_a940-route.

    select sdabw  bezei
    from tvsakt
    into table t_tvsakt
    for all entries in t_a940
    where sdabw eq t_a940-sdabw
    and   spras eq sy-langu.

    perform trata_dados_a940.

  endif.

endform.

form trata_dados_a934.

  loop at t_a934 into w_a934.
    w_saida-shtyp  =  w_a934-shtyp.
    w_saida-tdlnr  =  w_a934-tdlnr.
    w_saida-lzonea =  w_a934-lzonea.
    w_saida-lzonez =  w_a934-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

*    IF W_A933-ADD01 = '0000000001'.
*      W_SAIDA-ADD01  = 'Sim'.
*    ELSEIF W_A933-ADD01 = '0000000002'.
*      W_SAIDA-ADD01  = 'Não'.
*    ENDIF.

    read table t_konp into w_konp with key knumh = w_a934-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a934-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a934-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,
            w_tzont.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a934-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a934-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    w_saida-pstlza = w_a934-pstlza.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a934, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a938.

  loop at t_a938 into w_a938.
    w_saida-shtyp  =  w_a938-shtyp.
    w_saida-tdlnr  =  w_a938-tdlnr.
    w_saida-lzonea =  w_a938-lzonea.
    w_saida-lzonez =  w_a938-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

*    IF W_A933-ADD01 = '0000000001'.
*      W_SAIDA-ADD01  = 'Sim'.
*    ELSEIF W_A933-ADD01 = '0000000002'.
*      W_SAIDA-ADD01  = 'Não'.
*    ENDIF.

    read table t_konp into w_konp with key knumh = w_a938-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a938-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a938-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,
            w_tzont.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a938-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a938-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    w_saida-pstlza = w_a938-pstlza.
    w_saida-matnr  = w_a938-matnr.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a938, w_konp, w_tzont.

  endloop.

endform.


form trata_dados_a939.

  loop at t_a939 into w_a939.
    w_saida-shtyp  =  w_a939-shtyp.
    w_saida-tdlnr  =  w_a939-tdlnr.
    w_saida-lzonea =  w_a939-lzonea.
    w_saida-lzonez =  w_a939-lzonez.
    w_saida-ddtext =  w_dd02t-ddtext.

*    IF W_A933-ADD01 = '0000000001'.
*      W_SAIDA-ADD01  = 'Sim'.
*    ELSEIF W_A933-ADD01 = '0000000002'.
*      W_SAIDA-ADD01  = 'Não'.
*    ENDIF.

    read table t_konp into w_konp with key knumh = w_a939-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
    endif.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a939-lzonea.

    read table t_tzont into w_tzont with key zone1 = w_a939-lzonea.
    if sy-subrc = 0.
      w_saida-vtexta = w_tzont-vtext.
    endif.

    clear : t_tzont,
            w_tzont.

    select land1
           zone1
           vtext
      from tzont
      into table t_tzont
      where land1 = 'BR'
      and   zone1 = w_a939-lzonez.

    read table t_tzont into w_tzont with key zone1 = w_a939-lzonez.
    if sy-subrc = 0.
      w_saida-vtextz = w_tzont-vtext.
    endif.

    w_saida-vegr5 = w_a939-vegr5.

    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    clear: w_saida, w_a939, w_konp, w_tzont.

  endloop.

endform.

form trata_dados_a940.
  loop at t_a940 into w_a940.

    read table t_trolz into w_trolz with key route = w_a940-route.
    if sy-subrc = 0.
      w_saida-lzonea = w_trolz-azone.
      w_saida-lzonez = w_trolz-lzone.

      select land1 zone1 vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-azone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-azone.
      if sy-subrc = 0.
        w_saida-vtexta = w_tzont-vtext.
      endif.

      clear : t_tzont,  w_tzont.

      select land1 zone1  vtext
        from tzont
        into table t_tzont
        where land1 = 'BR'
        and   zone1 = w_trolz-lzone.

      read table t_tzont into w_tzont with key zone1 = w_trolz-lzone.
      if sy-subrc = 0.
        w_saida-vtextz = w_tzont-vtext.
      endif.
    endif.

    read table t_tvrot into w_tvrot with key route = w_a940-route.
    if sy-subrc = 0.
      w_saida-bezei =  w_tvrot-bezei.
    endif.

    w_saida-shtyp =  w_a940-shtyp.
    w_saida-tdlnr =  w_a940-tdlnr.
    w_saida-route =  w_a940-route.
    w_saida-ddtext = w_dd02t-ddtext.

    read table t_konp into w_konp with key knumh = w_a940-knumh.
    if sy-subrc = 0.
      w_saida-kbetr = w_konp-kbetr.
      w_saida-konwa = w_konp-konwa.
      w_saida-kmein = w_konp-kmein.
      if w_konp-krech = 'A' .
        w_saida-kbetr = w_saida-kbetr / 10.
      endif.
    endif.
    if w_saida-kbetr <> 0.
      append w_saida to t_saida.
    endif.

    w_saida-sdabw = w_a940-sdabw.
    read table t_tvsakt into data(w_tvsakt) with key sdabw = w_a940-sdabw.
    if sy-subrc = 0.
      w_saida-bezec =  w_tvsakt-bezei.
    endif.

    clear: w_saida, w_a940, w_konp, w_tzont.

  endloop.

endform.
