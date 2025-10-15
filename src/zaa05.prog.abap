*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 16/08/2010                                              &*
*& Descrição: Relatorio Inventario - Grupo Maggi                      &*
*& Transação: ZAA06                                                   &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*&--------------------------------------------------------------------&*

report  zaa05.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
type-pools: slis, kkblo.
*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
types: begin of ty_saida.
*         INCLUDE TYPE equi.
types:
  equnr      type equi-equnr,        "Equipamento          "User Story 153788 // MMSILVA - 08.10.2024
  anln1      type anla-anln1,
  anln2      type anla-anln2,
  txt50      type anla-txt50,
  txa50      type anla-txa50,
  anlhtxt    type anlh-anlhtxt,
  SERNR      TYPE ANLA-SERNR,
  INVNR      TYPE ANLA-INVNR,
  kostl      type anlz-kostl,
  cskt_ltext type cskt-ltext,         "Descr. Centro de Custo  "*-CS2022000951-01.02.2023-#93773-JT
  stort      type anlz-stort,
  raumn      type anlz-raumn,
  kfzkz      type anlz-kfzkz,
  aktiv      type anla-aktiv,
  deakt      type anla-deakt,
  lifnr      type anla-lifnr,
  liefe      type anla-liefe,
  ivdat      type anla-ivdat,
  bukrs      type anla-bukrs,
  gsber      type anlz-gsber,
  anlkl      type anla-anlkl,
  xva_brl    type anlc-kansw,
  xvda_brl   type anlc-knafa,
  xcont_brl  type anlc-knafa,
  xva_usd    type anlc-kansw,
  xvda_usd   type anlc-knafa,
  xcont_usd  type anlc-knafa,
  conta      type t095-ktansw,        "Conta        "*-CS2022000951-01.02.2023-#93773-JT
  desc_conta type skat-txt50,         "Descr. Conta "*-CS2022000951-01.02.2023-#93773-JT
*         EQUNR     TYPE EQUI-EQUNR.
  eqktx      type eqkt-eqktx,        "Desc. Equipamento          "User Story 153788 // MMSILVA - 08.10.2024
  kostl_iloa type iloa-kostl,        "Centro de Custo Equip.     "User Story 153788 // MMSILVA - 08.10.2024
  swerk      type iloa-swerk,        "Filial Equip.              "User Story 153788 // MMSILVA - 08.10.2024
  end of ty_saida,

  begin of ty_lfa1,
    lifnr type lfa1-lifnr,
    name1 type lfa1-name1,
  end of ty_lfa1,

  begin of ty_fleet_aux,
    anln1 type zzimobi,
    anln2 type anla-anln2,
  end of ty_fleet_aux,

  begin of ty_anlc,
    bukrs type anlc-bukrs,
    anln1 type anlc-anln1,
    anln2 type anlc-anln2,
    gjahr type anlc-gjahr,
    afabe type anlc-afabe,
    kansw type anlc-kansw,
    knafa type anlc-knafa,
    answl type anlc-answl,
    nafap type anlc-nafap,
  end of ty_anlc.

*-CS2022000978-31.01.2023-#93774-JT-inicio
types: begin of ty_conta,
         ktopl type t095-ktopl,
         ktogr type t095-ktogr,
         afabe type t095-afabe,
         saknr type skat-saknr,
         txt50 type skat-txt50.
types: end   of ty_conta.
*-CS2022000978-31.01.2023-#93774-JT-fim

*DATA: tg_anla     TYPE TABLE OF anla WITH HEADER LINE,
*      tg_anlh     TYPE TABLE OF anlh WITH HEADER LINE,
**      t_equi      TYPE TABLE OF v_equi,
*      tg_anlz     TYPE TABLE OF anlz WITH HEADER LINE,
*      tg_lfa1     TYPE TABLE OF ty_lfa1 WITH  HEADER LINE,
*      tg_anlc_br  TYPE TABLE OF ty_anlc WITH HEADER LINE,
*      tg_anlc_usd TYPE TABLE OF ty_anlc WITH HEADER LINE,
*      tg_saida    TYPE TABLE OF ty_saida WITH HEADER LINE,
data: it_anla      type table of anla with header line,
      it_fleet_aux type table of ty_fleet_aux,
      it_iloa_aux  type table of iloa,
      it_equz_aux  type table of equz,
      it_equi_aux  type table of equi,
      it_eqkt_aux  type table of eqkt,
      it_anlh      type table of anlh with header line,
*      t_equi      TYPE TABLE OF v_equi,
      it_anlz      type table of anlz with header line,
      it_lfa1      type table of ty_lfa1 with  header line,
      it_anlc_br   type table of ty_anlc with header line,
      it_anlc_usd  type table of ty_anlc with header line,
      it_saida     type table of ty_saida with header line,
      it_cskt      type table of cskt, "*-CS2022000951-01.02.2023-#93773-JT
      wa_cskt      type cskt,          "*-CS2022000951-01.02.2023-#93773-JT
      it_conta     type table of ty_conta, "*-CS2022000951-01.02.2023-#93773-JT
      wa_conta     type ty_conta, "*-CS2022000951-01.02.2023-#93773-JT
      it_eqkt      type table of eqkt,   "User Story 153788 // MMSILVA - 08.10.2024
      wa_eqkt      type eqkt,            "User Story 153788 // MMSILVA - 08.10.2024
      it_equz      type table of equz,   "User Story 153788 // MMSILVA - 08.10.2024
      wa_equz      type equz,            "User Story 153788 // MMSILVA - 08.10.2024
      it_iloa      type table of iloa,   "User Story 153788 // MMSILVA - 08.10.2024
      wa_iloa      type iloa,            "User Story 153788 // MMSILVA - 08.10.2024
      it_equi      type table of equi,   "User Story 153788 // MMSILVA - 08.10.2024
      wa_equi      type equi,            "User Story 153788 // MMSILVA - 08.10.2024
      it_fleet     type table of fleet,  "User Story 153788 // MMSILVA - 08.10.2024
      wa_fleet     type fleet,           "User Story 153788 // MMSILVA - 08.10.2024
*      it_anla     TYPE TABLE OF anla,   "User Story 153788 // MMSILVA - 08.10.2024

      wa_anla      type anla.

types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.
*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
data: xs_events    type slis_alv_event,
      events       type slis_t_event,
      t_print      type slis_print_alv,
      estrutura    type table of ty_estrutura,
      wa_estrutura type ty_estrutura,
      v_report     like sy-repid,
      t_top        type slis_t_listheader.


*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-001.
  select-options: s_bukrs for it_anla-bukrs obligatory,
                  s_gsber for it_anlz-gsber,
                  s_gjahr for it_anlc_br-gjahr obligatory,
                  s_anlkl for it_anla-anlkl,
                  s_anln1 for it_anla-anln1,
                  s_kostl for it_anlz-kostl.

  parameters p_valor as checkbox default ''.

selection-screen: end of block b1.
*----------------------------------------------------------------------*
* Start of selection
*----------------------------------------------------------------------*
start-of-selection.
  perform seleciona_dados.
  perform organiza_dados.
  perform imprimir_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados .

  select *
    from anlz
    into table it_anlz
     where bukrs in s_bukrs
       and anln1 in s_anln1
       and gsber in s_gsber
       and kostl in s_kostl
       and bdatu ge sy-datum.

  if sy-subrc is initial.
    select *
     from anla
     into table it_anla
      for all entries in it_anlz
      where bukrs eq it_anlz-bukrs
        and anln1 eq it_anlz-anln1
        and anlkl in s_anlkl.

    if sy-subrc is initial and p_valor is not initial.
      select lifnr name1
        from lfa1
        into table it_lfa1
         for all entries in it_anla
          where lifnr eq it_anla-lifnr.

      select bukrs   anln1
             anln2   gjahr
             afabe   kansw  knafa answl nafap
           from  anlc into table it_anlc_br
        for all entries in it_anla
       where  bukrs in s_bukrs
          and anln1 eq it_anla-anln1
          and anln2 eq it_anla-anln2
          and gjahr in s_gjahr
          and afabe eq '01'.

      select bukrs   anln1
             anln2   gjahr
             afabe   kansw  knafa answl nafap
           from  anlc into table it_anlc_usd
        for all entries in it_anla
       where  bukrs in s_bukrs
          and anln1 eq it_anla-anln1
          and anln2 eq it_anla-anln2
          and gjahr in s_gjahr
          and afabe eq '41'.
    endif.

    select *
      from anlh
      into table it_anlh
      for all entries in it_anlz
       where bukrs eq it_anlz-bukrs
         and anln1 eq it_anlz-anln1.

*   AJUSTE BUSCA EQUIPAMENTO // US 153788 - MMSILVA - 08.10.2024

*    loop at it_anla into data(ls_anla).

*      data: lv_anln1 type string.
*
*      lv_anln1 = ls_anla-anln1.
*
*      while lv_anln1 cp '0*'.
*        lv_anln1 = lv_anln1+1.
*      endwhile.
*
*
**      lv_anln1 = |{ lv_anln1 ALPHA = IN }|. "Adiciona zero esquerda.
*      lv_anln1 = |{ lv_anln1 alpha = out }|. "retira zero esquerda.
*
**      lv_current_length = strlen( lv_anln1 ).
**
**      IF lv_current_length < lv_length.
**        lv_spaces = ''.
**        DO lv_length - lv_current_length TIMES.
**          lv_spaces = lv_spaces && ' '.
**        ENDDO.
**
**        lv_anln1 = lv_spaces && lv_anln1.
**      ENDIF.
*
*      data it_temp_fleet type table of fleet.
*
*      select *
*      from fleet
*      into table @it_temp_fleet
*      where zzimobilizado = @lv_anln1.
*
*      append lines of it_temp_fleet to it_fleet.

*    endloop.

    free: it_fleet_aux.
    it_fleet_aux = value #( for ls_anla in it_anla ( anln1 = |{ ls_anla-anln1 alpha = out }|
                                                     anln2 = ls_anla-anln2 ) ).


    loop at it_anla into data(lw_anla).
      append value #( anln1 = lw_anla-anln1
                      anln2 = lw_anla-anln2 ) to it_fleet_aux.

    endloop.


    if it_fleet_aux is not initial.
      select * from fleet into table it_fleet
      for all entries in it_fleet_aux
        where zzimobilizado eq IT_fleet_aux-anln1.
      if it_fleet is not initial.
        select *
          from equi
          into table it_equi
          for all entries in it_fleet
          where objnr = it_fleet-objnr.
      endif.
    endif.
  endif.
*   AJUSTE BUSCA EQUIPAMENTO // US 153788 - MMSILVA - 08.10.2024

*-CS2022000951-01.02.2023-#93773-JT-inicio
  if it_anlz[] is not initial.
    select *
      from cskt
      into table it_cskt
       for all entries in it_anlz
     where spras = sy-langu
       and kostl =  it_anlz-kostl.
  endif.

  if it_anla[] is not initial.
    select t095~ktopl  t095~ktogr  t095~afabe
           skat~saknr  skat~txt50
      from t095
      inner join skat on skat~spras = sy-langu
                     and skat~ktopl = t095~ktopl
                     and skat~saknr = t095~ktansw
      into table it_conta
       for all entries in it_anla
     where t095~ktogr = it_anla-ktogr
       and t095~afabe = '01'.
  endif.

  sort it_cskt by kostl datbi descending.
  delete adjacent duplicates from it_cskt
                        comparing kostl.
*-CS2022000951-01.02.2023-#93773-JT-fim

* User Story 153788 // MMSILVA - 08.10.2024 - Inicio
  if it_equi is not initial.
    select *
      from eqkt
      into table it_eqkt
      for all entries in it_equi
      where equnr = it_equi-equnr
      and spras = sy-langu.

    select *
      from equz
      into table it_equz
      for all entries in it_equi
      where equnr = it_equi-equnr
      and datbi eq '99991231'.

    select *
      from iloa
      into table it_iloa
      for all entries in it_equz
      where iloan = it_equz-iloan.
  endif.


  if it_fleet_aux is not initial.
    free: it_iloa_aux.
    select * from iloa into table it_iloa_aux
      for all entries in it_fleet_aux
      where anlnr eq it_fleet_aux-anln1+0(12)
        and anlun eq it_fleet_aux-anln2.

    if it_iloa_aux is not initial.
      select *
        from equz
        into table it_equz_aux
        for all entries in it_iloa_aux
        where iloan = it_iloa_aux-iloan
        and datbi eq '99991231'.
      if it_equz_aux is not initial.
        select *
          from equi
          into table it_equi_aux
          for all entries in it_equz_aux
          where equnr = it_equz_aux-equnr.

        select *
        from eqkt
        into table it_eqkt_aux
        for all entries in it_equi_aux
        where equnr = it_equi_aux-equnr
          and spras = sy-langu.
      endif.
    endif.
  endif.
* User Story 153788 // MMSILVA - 08.10.2024 - Fim


endform.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form organiza_dados .
  loop at it_anla.

    free: wa_cskt, wa_conta.

    read table it_anlz
      with key bukrs = it_anla-bukrs
               anln1 = it_anla-anln1
               anln2 = it_anla-anln2.

    if sy-subrc is initial.
      read table it_anlh
        with key bukrs = it_anla-bukrs
                 anln1 = it_anla-anln1.

      read table it_lfa1
        with key lifnr = it_anla-lifnr.

      move-corresponding: it_anlz to it_saida,
                          it_anla to it_saida,
                          it_anlh to it_saida.

      move: it_lfa1-name1  to it_saida-liefe.

      read table it_anlc_br with key anln1 = it_anlz-anln1
                                     anln2 = it_anlz-anln2.

      it_saida-xva_brl  = it_anlc_br-kansw + it_anlc_br-answl.
      it_saida-xvda_brl = it_anlc_br-knafa + it_anlc_br-nafap.

      it_saida-xcont_brl = ( it_saida-xva_brl + it_saida-xvda_brl ). "TG_ANLC_BR-KANSW + TG_ANLC_BR-KNAFA


      read table it_anlc_usd with key anln1 = it_anlz-anln1
                                      anln2 = it_anlz-anln2.

      it_saida-xva_usd   = it_anlc_usd-kansw + it_anlc_usd-answl.
      it_saida-xvda_usd  = it_anlc_usd-knafa + it_anlc_usd-nafap.

      it_saida-xcont_usd = ( it_saida-xva_usd + it_saida-xvda_usd ). "TG_ANLC_USD-KANSW + TG_ANLC_USD-KNAFA

**     AJUSTE BUSCA EQUIPAMENTO // US 153788 - MMSILVA - 08.10.2024
*      while it_anla-anln1 cp '0*'.
*        it_anla-anln1 = it_anla-anln1+1.
*      endwhile.
*
*      read table it_fleet into wa_fleet with key zzimobilizado = it_anla-anln1.
*
*      if it_anla-anln1 is not initial.
*        read table it_equi into data(wa_equi) with key objnr = wa_fleet-objnr.
*        if sy-subrc eq 0.
*          it_saida-equnr = wa_equi-equnr.
*        endif.
*      endif.
**     AJUSTE BUSCA EQUIPAMENTO // US 153788 - MMSILVA - 08.10.2024

*-CS2022000951-01.02.2023-#93773-JT-inicio
      try.
          wa_cskt  = it_cskt[ kostl = it_anlz-kostl ].
        catch cx_sy_itab_line_not_found into data(l_error).
      endtry.

      try.
          wa_conta  = it_conta[ ktogr = it_anla-ktogr ].
        catch cx_sy_itab_line_not_found into l_error.
      endtry.

      it_saida-cskt_ltext = wa_cskt-ltext.
      it_saida-conta      = wa_conta-saknr.
      it_saida-desc_conta = wa_conta-txt50.
*-CS2022000951-01.02.2023-#93773-JT-inicio

*
*AJUSTE BUSCA EQUIPAMENTO // US 153788 - MMSILVA - 08.10.2024
      clear: wa_fleet, wa_iloa, wa_equi, wa_eqkt.
      if it_anla-anln2 eq 0.
        read table it_fleet into wa_fleet with key zzimobilizado = |{ it_anla-anln1 alpha = out }|.
      endif.

      if sy-subrc eq 0.
        read table it_equi into wa_equi with key objnr = wa_fleet-objnr.

        it_saida-equnr = wa_equi-equnr.

        read table it_eqkt into wa_eqkt with key equnr = wa_equi-equnr.

        it_saida-eqktx = wa_eqkt-eqktx.

        read table it_equz into wa_equz with key equnr = wa_equi-equnr
                                                 datbi = '99991231'.

        read table it_iloa into wa_iloa with key iloan = wa_equz-iloan.

        it_saida-kostl_iloa = wa_iloa-kostl.
        it_saida-swerk      = wa_iloa-swerk.

      else.

        read table it_iloa_aux into wa_iloa with key anlnr = it_anla-anln1
                                                     anlun = it_anla-anln2.
        if sy-subrc = 0.

          it_saida-kostl_iloa = wa_iloa-kostl.
          it_saida-swerk = wa_iloa-swerk.

          read table it_equz_aux into wa_equz with key iloan = wa_iloa-iloan
                                                     datbi = '99991231'.
          if sy-subrc = 0.
            read table it_equi_aux into wa_equi with key equnr = wa_equz-equnr.
            if sy-subrc eq 0.
              it_saida-equnr = wa_equi-equnr.
              read table it_eqkt_aux into wa_eqkt with key equnr = wa_equi-equnr.
              if sy-subrc eq 0.
                it_saida-eqktx = wa_eqkt-eqktx.
              endif.
            endif.
          endif.
        endif.
      endif.

      append it_saida.
      clear: it_saida.

    endif.
    clear: it_anlz, it_anlh, it_anlc_usd, it_anlc_br, it_anla, wa_fleet, wa_equi.
  endloop.

* User Story 153788 // MMSILVA - 08.10.2024 - Inicio

  data: lt_new_values type table of anla, " Altere 'your_structure' para o tipo adequado
        ls_new_values type anla.

*  loop at it_saida assigning field-symbol(<_new_values>).
*
*    read table it_fleet into wa_fleet with key zzimobilizado = it_saida-anln1.
*
*    if sy-subrc eq 0.
*      read table it_equi into wa_equi with key objnr = wa_fleet-objnr.
*
*      <_new_values>-equnr = wa_equi-equnr.
*
*      read table it_eqkt into wa_eqkt with key equnr = wa_equi-equnr.
*
*      <_new_values>-eqktx = wa_eqkt-eqktx.
*
*      read table it_equz into wa_equz with key equnr = wa_equi-equnr
*                                               datbi = '99991231'.
*
*      read table it_iloa into wa_iloa with key iloan = wa_equz-iloan.
*
*      <_new_values>-kostl_iloa = wa_iloa-kostl.
*      <_new_values>-swerk      = wa_iloa-swerk.
*
*
*    endif.
*  endloop.
* User Story 153788 // MMSILVA - 08.10.2024 - Fim
endform.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form imprimir_dados.

*  PERFORM DEFINIR_EVENTOS.
  perform montar_layout.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = sy-repid
      i_callback_user_command = 'XUSER_COMMAND'
      it_fieldcat             = estrutura[]
      i_save                  = 'A'
      it_events               = events
      is_print                = t_print
    tables
      t_outtab                = it_saida.

endform.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout.

  data: l_valor type c.

  if p_valor is not initial.
    l_valor = ' '.
  else.
    l_valor = 'X'.
  endif.

  perform montar_estrutura using:
        1 'ANLA'        'ANLN1'         'IT_SAIDA'      'ANLN1'         ' '                       ' '     'L'     ' ',
        2 'ANLA'        'ANLN2'         'IT_SAIDA'      'ANLN2'         ' '                       ' '     'L'     ' ',
        3 'EQUNR'       'EQUNR'         'IT_SAIDA'      'EQUNR'         'Equipamento '            ' '     'L'     ' ',
        4 'ANLA'        'TXT50'         'IT_SAIDA'      'TXT50'         ' '                       ' '     'L'     ' ',
        5 'ANLA'        'TXA50'         'IT_SAIDA'      'TXA50'         ' '                       ' '     'L'     ' ',
        6 'ANLH'        'ANLHTXT'       'IT_SAIDA'      'ANLHTXT'       ' '                       ' '     'L'     ' ',
        7 'ANLA'        'SERNR'         'IT_SAIDA'      'SERNR'         ' '                       ' '     'L'     ' ',
        8 'ANLA'        'INVNR'         'IT_SAIDA'      'INVNR'         ' '                       ' '     'L'     ' ',
        9 'ANLZ'        'KOSTL'         'IT_SAIDA'      'KOSTL'         ' '                       ' '     'L'     ' ',
       10 'CSKT'        'LTEXT'         'IT_SAIDA'      'CSKT_LTEXT'    'Descrição C.Custo'       ' '     'L'     ' ',      "*-CS2022000951-01.02.2023-#93773-JT
       11 'ANLZ'        'STORT'         'IT_SAIDA'      'STORT'         ' '                       ' '     'L'     ' ',
       12 'ANLZ'        'RAUMN'         'IT_SAIDA'      'RAUMN'         ' '                       ' '     'L'     ' ',
       13 'ANLZ'        'KFZKZ'         'IT_SAIDA'      'KFZKZ'         ' '                       ' '     'L'     ' ',
       14 'ANLA'        'AKTIV'         'IT_SAIDA'      'AKTIV'         ' '                       ' '     'L'     ' ',
       15 'ANLA'        'DEAKT'         'IT_SAIDA'      'DEAKT'         ' '                       ' '     'L'     ' ',
       16 'ANLA'        'LIFNR'         'IT_SAIDA'      'LIFNR'         ' '                       ' '     'L'     ' ',
       17 'ANLA'        'LIEFE'         'IT_SAIDA'      'LIEFE'         ' '                       ' '     'L'     ' ',
       18 'ANLA'        'IVDAT'         'IT_SAIDA'      'IVDAT'         ' '                       ' '     'L'     ' ',
       19 'ANLA'        'BUKRS'         'IT_SAIDA'      'BUKRS'         ' '                       ' '     'L'     ' ',
       20 'ANLZ'        'GSBER'         'IT_SAIDA'      'GSBER'         ' '                       ' '     'L'     ' ',
       21 'ANLA'        'ANLKL'         'IT_SAIDA'      'ANLKL'         ' '                       ' '     'L'     ' ',
       22 'ANLC'        'KANSW'         'IT_SAIDA'      'XVA_BRL'       'Vlr.Aquisição BRL'       ' '     'R'     l_valor,
       23 'ANLC'        'KANSW'         'IT_SAIDA'      'XVDA_BRL'      'Vlr.Deprec.Acum.BRL'     ' '     'R'     l_valor,
       24 'ANLC'        'KANSW'         'IT_SAIDA'      'XCONT_BRL'     'Vlr.Contabil BRL'        ' '     'R'     l_valor,
       25 'ANLC'        'KANSW'         'IT_SAIDA'      'XVA_USD'       'Vlr.Aquisição USD'       ' '     'R'     l_valor,
       26 'ANLC'        'KANSW'         'IT_SAIDA'      'XVDA_USD'      'Vlr.Deprec.Acum.USD'     ' '     'R'     l_valor,
       27 'ANLC'        'KANSW'         'IT_SAIDA'      'XCONT_USD'     'Vlr.Contabil USD'        ' '     'R'     l_valor,
       29 'SKAT'        'TXT50'         'IT_SAIDA'      'DESC_CONTA'    'Descrição da Conta'      ' '     'L'     ' ',        "*-CS2022000951-01.02.2023-#93773-JT
       28 'T095'        'KTANSW'        'IT_SAIDA'      'CONTA'         'Conta'                   '7'     'L'     ' ',        "*-CS2022000951-01.02.2023-#93773-JT
       30 'EQKTX'       'EQKT'          'IT_SAIDA'      'EQKTX'         'Descrição Equipamento'   ' '     'L'     ' ',        "   User Story 153788 // MMSILVA - 08.10.2024
       31 'ILOA'        'KOSTL_ILOA'    'IT_SAIDA'      'KOSTL_ILOA'    'Centro de Custo Equip'   ' '     'L'     ' ',        "   User Story 153788 // MMSILVA - 08.10.2024
       32 'ILOA'        'SWERK'         'IT_SAIDA'      'SWERK'         'Filial Equipamento'      ' '     'L'     ' '.        "   User Story 153788 // MMSILVA - 08.10.2024



endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_just)
                            value(p_out).


  clear wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = p_out.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-just          = p_just." 'L'.

*-CS2022000951-01.02.2023-#93773-JT-inicio
  if p_field = 'CONTA'.
    wa_estrutura-ddictxt     = abap_off.
  endif.
*-CS2022000951-01.02.2023-#93773-JT-fim

  if p_field eq 'ANLN1'.
    wa_estrutura-hotspot     = 'X'.
  elseif p_field eq 'EQUNR'.
    wa_estrutura-hotspot     = 'X'.
  endif.

  append wa_estrutura to estrutura.
  clear: wa_estrutura.
endform.                    " montar_estrutura
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form xuser_command using ucomm like sy-ucomm
                         selfield type kkblo_selfield.
  selfield = selfield.                                      "#EC CALLED
  case ucomm.
    when '&IC1'.
* Lê na tabela de saída
      read table it_saida index selfield-tabindex.

      if sy-subrc eq 0.
* Se foi clicado na coluna EBELN.
        if selfield-fieldname = 'ANLN1'.
* Passa o valor clicado na coluna como parâmetro para a transação que
*se quer chamar.
* Passa o id do campo Pedido na transação ME23N.
          set parameter id 'AN1' field it_saida-anln1.
          set parameter id 'AN2' field it_saida-anln2.
          set parameter id 'BUK' field it_saida-bukrs.
* Chamo a transação
          call transaction 'AS03' and skip first screen.
        elseif selfield-fieldname = 'EQUNR'.
          set parameter id 'EQN' field it_saida-equnr.

          call transaction 'IE03' and skip first screen.
        endif.
      endif.
    when others.
  endcase.

endform. "XUSER_COMMAND
