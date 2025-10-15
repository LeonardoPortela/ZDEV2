*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 29/05/2012                                              &*
*& Descrição: Conversor de arquivo Fcont                              &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK922306   29.05.2012                            &*
*&--------------------------------------------------------------------&*

report  zfcont.
tables: bsis.

**--------------------------------------------------------------------**
**                       Declaração - Tabelas Internas
**--------------------------------------------------------------------**
data: begin of t_imp occurs 0,
        field(1000),
      end of t_imp,

      begin of t_split occurs 0,
       campo(100),
      end of t_split,

      begin of t_contas occurs 0,
       saknr type bsis-hkont,
       perio type c,
      end of t_contas,

      begin of t_faglflext occurs 0,
       racct type faglflext-racct,
       drcrk type faglflext-drcrk,
       hslvt type faglflext-hslvt,
       hsl01 type faglflext-hsl01,
       hsl02 type faglflext-hsl02,
       hsl03 type faglflext-hsl03,
       hsl04 type faglflext-hsl04,
       hsl05 type faglflext-hsl05,
       hsl06 type faglflext-hsl06,
       hsl07 type faglflext-hsl07,
       hsl08 type faglflext-hsl08,
       hsl09 type faglflext-hsl09,
       hsl10 type faglflext-hsl10,
       hsl11 type faglflext-hsl11,
       hsl12 type faglflext-hsl12,
       hsl13 type faglflext-hsl13,
       hsl14 type faglflext-hsl14,
       hsl15 type faglflext-hsl15,
       hsl16 type faglflext-hsl16,
      end of t_faglflext,

      t_out like table of t_imp with header line,
      t_imp_i200 like table of t_imp with header line.
**--------------------------------------------------------------------**
**                       Declaração - Variaveis globais
**--------------------------------------------------------------------**
data: x_filename           type string,
      wl_tabix             type sy-tabix,
      x_msgerro            type sy-lisel,
      wg_periodo(3)        type c,
      wg_file              type rlgrap-filename,
      ok-code              type sy-ucomm value 'LOCAL'.
**--------------------------------------------------------------------**
**                       Tela de Seleção
**--------------------------------------------------------------------**
selection-screen: begin of block a1 with frame title text-001.
select-options: s_bukrs for bsis-bukrs,
                s_hkont for bsis-hkont,
                s_comp for bsis-hkont,
                s_gjahr for bsis-gjahr.

selection-screen: begin of block b1 with frame title text-012.
parameters: p_anual  radiobutton group a1,
            p_trim   radiobutton group a1.
*            p_2trim  radiobutton group a1,
*            p_3trim  radiobutton group a1.

selection-screen : end of block b1.

*--> Arquivo
selection-screen: begin of block b2 with frame title text-011.
selection-screen: begin of block b3 with frame title text-013.
parameters: p_local  radiobutton group a2 default 'X' user-command asd,
            p_unix   radiobutton group a2.

selection-screen : end of block b3.

selection-screen: begin of block b4 with frame title text-014.
parameters: p_util    radiobutton group a4 default 'X', "user-command ASD,
            p_busca   radiobutton group a4.

selection-screen : end of block b4.

parameters: p_f_in(250)  type c obligatory default 'C:\',
            p_f_aux(250)  type c no-display,
            p_f_out(250)  type c obligatory default 'C:\'.
selection-screen : end of block b2.
*<--
selection-screen : end of block a1.


at selection-screen.
  perform f_modifica_tela.

at selection-screen on value-request for p_f_in.
  perform f_abre_arquivo using 'in'.

*at selection-screen on value-request for p_f_aux.
*  perform f_abre_arquivo using 'aux'.

at selection-screen on value-request for p_f_out.
  perform f_abre_arquivo using 'out'.

start-of-selection.
  case 'X'.
    when p_local.
      perform ler_txt_local tables t_imp
                            using p_f_in.

      if p_f_aux is not initial.
        perform ler_txt_local tables t_imp_i200
                              using p_f_aux.
      endif.
      if t_imp[] is not initial.
        if p_busca is not initial.
          perform seleciona_dados_i200.
        endif.
        perform converte_arquivo.
        perform exporta_arquivo.
      endif.
    when p_unix.
      perform ler_txt_unix tables t_imp
                           using p_f_in.
      if p_f_aux is not initial.
        perform ler_txt_unix tables t_imp_i200
                             using p_f_aux.
      endif.
      if t_imp[] is not initial.
        if p_busca is not initial.
          perform seleciona_dados_i200.
        endif.
        perform converte_arquivo.
        perform exporta_arquivo_unix.
      endif.
  endcase.
*&---------------------------------------------------------------------*
*&      Form  F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_abre_arquivo using p_file.
*  IF P_LOCAL EQ C_X.
*Funcao para abri "Disco Local"
  if p_local is not initial.
    call function 'KD_GET_FILENAME_ON_F4'
         exporting
              field_name    = 'C:\'
* FC - UPGRADE ECC 6.0 - LUP xxx - Início
*            MASK          = '.txt '
* FC - UPGRADE ECC6.0 - LUP xxx - Fim
         changing
              file_name     = wg_file
         exceptions
              mask_too_long = 1
              others        = 2.
    case sy-subrc.
      when 1.
        message e000(zb) with 'Nome do arquivo he muito longo.'.
      when 2.
        message e000(zb) with 'Ocorreu um erro.'.
    endcase.
    if p_file eq 'in'.
      p_f_in = wg_file.
    elseif p_file eq 'aux'.
      p_f_aux = wg_file.
    elseif p_file eq 'out'.
      p_f_out = wg_file.
    endif.

  endif.
endform.                    " F_ABRE_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  LER_TXT_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ler_txt_local tables tl_imp
                   using p_file.
*  x_filename = p_f_in.
  x_filename = p_file.

  call function 'GUI_UPLOAD'
    exporting
      filename                = x_filename
      filetype                = 'ASC'
      read_by_line            = 'X'
    tables
      data_tab                = tl_imp[]
    exceptions
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      others                  = 17.

  if sy-subrc ne 0.
    message i000(zb) with 'Erro na importação do arquivo.'.
    leave list-processing.
    set screen 0.
  else.
  endif.

endform.                    "LER_TXT_LOCAL
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form converte_arquivo .
  data: begin of tl_lancamentos occurs 0,
        field(1000),
        flag,
        end of tl_lancamentos,

        begin of tl_sperio occurs 0,
         saknr type ska1-saknr,
         dperio type faglflext-hsl01,
         cperio type faglflext-hsl01,
         sfinal type faglflext-hsl01,
         tipo   type c,
         perio  type c,
        end of tl_sperio.

  data: wl_valor_a type faglflext-hsl01,
        wl_valor_1 type faglflext-hsl01,
        wl_valor_2 type faglflext-hsl01,
        wl_valor_3 type faglflext-hsl01,
        wl_valor_4 type faglflext-hsl01,
        wl_valor_i type faglflext-hsl01,
        wl_valor_c type faglflext-hsl01,
        wl_valor_d type faglflext-hsl01,
        wl_valor_f type faglflext-hsl01,
        wl_valor_aux(20),
        wl_linhas type sy-tabix,
        wl_linhas_aux(6) type c,
        wl_cont type sy-tabix,
        wl_200_l type sy-tabix,
        wl_150_l type sy-tabix,
        wl_155_flag type c,
        wl_data_aux type sy-datum,
        wl_data_aux2 type sy-datum,
        tl_out_aux  like table of t_out with header line,
        tl_imp_aux  like table of t_imp with header line,
        tl_ska1     type table of ska1 with header line,
        tl_ska1_24000 type table of ska1 with header line,
        tl_faglflext_155 like table of t_faglflext with header line,
        tl_faglflext_155_24000 like table of t_faglflext with header line,
        tl_split_aux like table of t_split with header line,
        tl_sperio_e like table of tl_sperio with header line.
*        TL_LANCAMENTO LIKE TABLE OF T_OUT WITH HEADER LINE.

  tl_imp_aux[] = t_imp[].
  delete tl_imp_aux where field+1(4) ne 'I155'.
****> Pega contas para selecionar em tabela FAGLFLEXT
*  LOOP AT TL_IMP_AUX.
*    SPLIT TL_IMP_AUX AT '|' INTO TABLE T_SPLIT.
*    DELETE T_SPLIT INDEX 1.
*    CLEAR: T_SPLIT, T_CONTAS.
*    READ TABLE T_SPLIT INDEX 2.
*    IF T_SPLIT-CAMPO IS NOT  INITIAL.
*      MOVE: T_SPLIT-CAMPO TO T_CONTAS-HKONT.
*      APPEND T_CONTAS.
*      CLEAR T_CONTAS.
*    ENDIF.
*  ENDLOOP.

  select * "#EC CI_DB_OPERATION_OK[2389136]
      from ska1 "#EC CI_DB_OPERATION_OK[2431747]
      into table tl_ska1
       where ktopl eq '0050'
         and ktoks eq 'YB01' " 'YB02', 'YB03', 'YB04').
          or   ktoks eq 'YB02'
          or   ktoks eq 'YB03'
          or   ktoks eq 'YB04' .

  select racct drcrk hslvt
         hsl01 hsl02 hsl03 hsl04 hsl05
         hsl06 hsl07 hsl08 hsl09 hsl10
         hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
    from faglflext
    into table tl_faglflext_155
    for all entries in tl_ska1
     where ryear in s_gjahr
*       AND RTCUR EQ 'BRL'
       and rldnr eq '0L'
       and racct eq tl_ska1-saknr
       and rbukrs in s_bukrs.


  select * "#EC CI_DB_OPERATION_OK[2431747]
    from ska1 "#EC CI_DB_OPERATION_OK[2389136]
    into table tl_ska1_24000
     where ktopl eq '0050'
       and  ktoks eq 'YB05'
         or   ktoks eq 'YB06'
         or   ktoks eq 'YB07'
         or   ktoks eq 'YB08' .

  select racct drcrk hslvt
       hsl01 hsl02 hsl03 hsl04 hsl05
       hsl06 hsl07 hsl08 hsl09 hsl10
       hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
  from faglflext
  into table tl_faglflext_155_24000
  for all entries in tl_ska1_24000
   where ryear in s_gjahr
*     AND RTCUR EQ 'BRL'
     and rldnr eq '0L'
     and racct eq tl_ska1_24000-saknr
     and rbukrs in s_bukrs.

  tl_imp_aux[] = t_imp[].
  delete tl_imp_aux where field+1(4) ne 'I200'
                      and field+1(4) ne 'I250'.

  refresh: t_out, t_contas, t_faglflext, tl_split_aux, tl_sperio,
           tl_sperio_e.
  loop at t_imp.
    wl_tabix = sy-tabix.
    split t_imp at '|' into table t_split.
    delete t_split index 1.
    if t_split[] is not initial.

      read table t_split index 1.
      if sy-subrc is initial.
**> Posição 0000
        if t_split-campo eq '0000'.
****> Posição 2 -  LALU
          clear t_split.
          move 'LALU' to t_split-campo.
          modify t_split index 2.

****> Posição 12 -  0
          clear t_split.
          move '0|' to t_split-campo.
          insert t_split index 12.
          clear t_imp.
          loop at t_split.
            if sy-tabix eq 1.
              concatenate '|'  t_split-campo into t_imp-field.
            else.
              concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
            endif.
          endloop.
          append t_imp to t_out.
        elseif t_split-campo eq 'I150'.
          case 'X'.
            when p_anual.
              read table t_out transporting no fields
                with key field+1(4) = 'I150'.
              if sy-subrc is not initial.
                split t_imp at '|' into table t_split.
                delete t_split index 1.
****> inicio do ano posição I150
                concatenate '0101' s_gjahr-low into t_split-campo.
                modify t_split index 2.
****> fim do ano posição I150
                concatenate '3112' s_gjahr-low into t_split-campo.
                modify t_split index 3.

                loop at t_split.
                  if sy-tabix eq 1.
                    concatenate '|'  t_split-campo into t_imp-field.
                  else.
                    concatenate t_imp-field '|' t_split-campo into t_imp-field.
                  endif.
                  at last.
                    concatenate t_imp-field '|' into t_imp-field.
                  endat.
                endloop.
                append t_imp to t_out.
                wl_150_l = sy-tabix.
                clear: wl_155_flag.
              endif.
            when p_trim.
              read table t_split index 2.
              if t_split-campo+2(2) eq '01'
              or t_split-campo+2(2) eq '04'
              or t_split-campo+2(2) eq '07'
              or t_split-campo+2(2) eq '10'.
                split t_imp at '|' into table t_split.
                delete t_split index 1.
****> inicio do ano posição I150
                concatenate '01' t_split-campo+2(2) s_gjahr-low into t_split-campo.
                modify t_split index 2.


****> fim do ano posição I150
                if t_split-campo+2(2) eq '01'.
                  concatenate s_gjahr-low '0301' into wl_data_aux.
                elseif t_split-campo+2(2) eq '04'.
                  concatenate s_gjahr-low '0601' into wl_data_aux.
                elseif t_split-campo+2(2) eq '07'.
                  concatenate s_gjahr-low '0901' into wl_data_aux.
                elseif t_split-campo+2(2) eq '10'.
                  concatenate s_gjahr-low '1201' into wl_data_aux.
                endif.
                call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
                  exporting
                    day_in            = wl_data_aux
                  importing
                    last_day_of_month = wl_data_aux2
                  exceptions
                    day_in_not_valid  = 1
                    others            = 2.

                concatenate wl_data_aux2+6(2) wl_data_aux2+4(2) wl_data_aux2(4) into t_split-campo.
                modify t_split index 3.

                loop at t_split.
                  if sy-tabix eq 1.
                    concatenate '|'  t_split-campo into t_imp-field.
                  else.
                    concatenate t_imp-field '|' t_split-campo into t_imp-field.
                  endif.
                  at last.
                    concatenate t_imp-field '|' into t_imp-field.
                  endat.
                endloop.
                append t_imp to t_out.
                wl_150_l = sy-tabix.
                clear: wl_155_flag.
              endif.

          endcase.
        elseif t_split-campo eq 'I155'.
          if wl_150_l is not initial
          and wl_155_flag is initial.
*              IF SY-SUBRC IS INITIAL.
            loop at tl_ska1.
              if s_comp[] is not initial.
                if tl_ska1-saknr in s_comp.
                  continue.
                endif.
              endif.
              clear: wl_valor_i, wl_valor_d, wl_valor_c, wl_valor_f.
              loop at tl_faglflext_155
                where racct eq tl_ska1-saknr.

                clear : tl_sperio.

                case 'X'.
                  when p_anual.
                    add tl_faglflext_155-hslvt to wl_valor_i.
                    if tl_faglflext_155-drcrk eq 'S'.
                      add tl_faglflext_155-hsl01 to wl_valor_d.
                      add tl_faglflext_155-hsl02 to wl_valor_d.
                      add tl_faglflext_155-hsl03 to wl_valor_d.
                      add tl_faglflext_155-hsl04 to wl_valor_d.
                      add tl_faglflext_155-hsl05 to wl_valor_d.
                      add tl_faglflext_155-hsl06 to wl_valor_d.
                      add tl_faglflext_155-hsl07 to wl_valor_d.
                      add tl_faglflext_155-hsl08 to wl_valor_d.
                      add tl_faglflext_155-hsl09 to wl_valor_d.
                      add tl_faglflext_155-hsl10 to wl_valor_d.
                      add tl_faglflext_155-hsl11 to wl_valor_d.
                      add tl_faglflext_155-hsl12 to wl_valor_d.
                      add tl_faglflext_155-hsl13 to wl_valor_d.
                      add tl_faglflext_155-hsl14 to wl_valor_d.
                      add tl_faglflext_155-hsl15 to wl_valor_d.
                      add tl_faglflext_155-hsl16 to wl_valor_d.
                    elseif tl_faglflext_155-drcrk eq 'H'.
                      add tl_faglflext_155-hsl01 to wl_valor_c.
                      add tl_faglflext_155-hsl02 to wl_valor_c.
                      add tl_faglflext_155-hsl03 to wl_valor_c.
                      add tl_faglflext_155-hsl04 to wl_valor_c.
                      add tl_faglflext_155-hsl05 to wl_valor_c.
                      add tl_faglflext_155-hsl06 to wl_valor_c.
                      add tl_faglflext_155-hsl07 to wl_valor_c.
                      add tl_faglflext_155-hsl08 to wl_valor_c.
                      add tl_faglflext_155-hsl09 to wl_valor_c.
                      add tl_faglflext_155-hsl10 to wl_valor_c.
                      add tl_faglflext_155-hsl11 to wl_valor_c.
                      add tl_faglflext_155-hsl12 to wl_valor_c.
                      add tl_faglflext_155-hsl13 to wl_valor_c.
                      add tl_faglflext_155-hsl14 to wl_valor_c.
                      add tl_faglflext_155-hsl15 to wl_valor_c.
                      add tl_faglflext_155-hsl16 to wl_valor_c.
                    endif.
                  when p_trim.
                    read table t_out index wl_150_l.
                    split t_out at '|' into table tl_split_aux.
                    delete tl_split_aux index 1.
                    read table tl_split_aux index 2.

******> Primeiro Trimeste.
                    if tl_split_aux-campo+2(2) eq '01'
                    or tl_split_aux-campo+2(2) eq '02'
                    or tl_split_aux-campo+2(2) eq '03'.
*******> Pega valor inicial da conta
                      add tl_faglflext_155-hslvt to wl_valor_i.
                      if tl_faglflext_155-drcrk eq 'S'.
                        add tl_faglflext_155-hsl01 to wl_valor_d.
                        add tl_faglflext_155-hsl02 to wl_valor_d.
                        add tl_faglflext_155-hsl03 to wl_valor_d.
                      elseif tl_faglflext_155-drcrk eq 'H'.
                        add tl_faglflext_155-hsl01 to wl_valor_c.
                        add tl_faglflext_155-hsl02 to wl_valor_c.
                        add tl_faglflext_155-hsl03 to wl_valor_c.
                      endif.
                      tl_sperio-perio = '1'.
******> Segundo Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '04'
                       or  tl_split_aux-campo+2(2) eq '05'
                       or  tl_split_aux-campo+2(2) eq '06'.
                      read table tl_sperio
                        with key saknr = tl_faglflext_155-racct
                                 perio = '1'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.
                      if tl_faglflext_155-drcrk eq 'S'.
                        add tl_faglflext_155-hsl04 to wl_valor_d.
                        add tl_faglflext_155-hsl05 to wl_valor_d.
                        add tl_faglflext_155-hsl06 to wl_valor_d.
                      elseif tl_faglflext_155-drcrk eq 'H'.
                        add tl_faglflext_155-hsl04 to wl_valor_c.
                        add tl_faglflext_155-hsl05 to wl_valor_c.
                        add tl_faglflext_155-hsl06 to wl_valor_c.
                      endif.
                      tl_sperio-perio = '2'.
******> Terceiro Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '07'
                       or  tl_split_aux-campo+2(2) eq '08'
                       or  tl_split_aux-campo+2(2) eq '09'.
                      read table tl_sperio
                         with key saknr = tl_faglflext_155-racct
                                  perio = '2'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155-drcrk eq 'S'.
                        add tl_faglflext_155-hsl07 to wl_valor_d.
                        add tl_faglflext_155-hsl08 to wl_valor_d.
                        add tl_faglflext_155-hsl09 to wl_valor_d.
                      elseif tl_faglflext_155-drcrk eq 'H'.
                        add tl_faglflext_155-hsl07 to wl_valor_c.
                        add tl_faglflext_155-hsl08 to wl_valor_c.
                        add tl_faglflext_155-hsl09 to wl_valor_c.
                      endif.
                      tl_sperio-perio = '3'.
******> Quarto Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '10'
                       or  tl_split_aux-campo+2(2) eq '11'
                       or  tl_split_aux-campo+2(2) eq '12'.
                      read table tl_sperio
                        with key saknr = tl_faglflext_155-racct
                                 perio = '3'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155-drcrk eq 'S'.
                        add tl_faglflext_155-hsl10 to wl_valor_d.
                        add tl_faglflext_155-hsl11 to wl_valor_d.
                        add tl_faglflext_155-hsl12 to wl_valor_d.
                        add tl_faglflext_155-hsl13 to wl_valor_d.
                        add tl_faglflext_155-hsl14 to wl_valor_d.
                        add tl_faglflext_155-hsl15 to wl_valor_d.
                      elseif tl_faglflext_155-drcrk eq 'H'.
                        add tl_faglflext_155-hsl10 to wl_valor_c.
                        add tl_faglflext_155-hsl11 to wl_valor_c.
                        add tl_faglflext_155-hsl12 to wl_valor_c.
                        add tl_faglflext_155-hsl13 to wl_valor_c.
                        add tl_faglflext_155-hsl14 to wl_valor_c.
                        add tl_faglflext_155-hsl15 to wl_valor_c.
                      endif.
                      tl_sperio-perio = '4'.
                    endif.
                endcase.
              endloop.
              if sy-subrc is not initial.
                continue.
              endif.

****> Posição I155 -> tipo de valor inicial> bloco 5
              if wl_valor_i ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 5.

****> Posição I155 -> Valor de Debito> bloco 6
              if wl_valor_d lt 0.
                multiply wl_valor_d by -1.
              endif.
              move wl_valor_d to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 6.

****> Posição I155 -> Valor de Credito> bloco 7
              if wl_valor_c lt 0.
                multiply wl_valor_c by -1.
              endif.
              move wl_valor_c to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 7.
****> Posição I155 -> Valor final calculo
              wl_valor_f = wl_valor_i + wl_valor_d - wl_valor_c.

****> Posição I155 -> Valor inicial da conta> bloco 4
              if wl_valor_i lt 0.
                multiply wl_valor_i by -1.
              endif.
              move wl_valor_i to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 4.

****> Posição I155 -> tipo de valor final> bloco 9
              if wl_valor_f ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              if wl_valor_f lt 0.
                multiply wl_valor_f by -1.
              endif.
              move wl_valor_f to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 8.

              move tl_ska1-saknr to t_split-campo.
              modify t_split index 2.

              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              move: wl_valor_f    to tl_sperio-sfinal,
                    wl_valor_c    to tl_sperio-cperio,
                    wl_valor_d    to tl_sperio-dperio,
                    tl_ska1-saknr to tl_sperio-saknr.
              read table t_split index 9.
              move t_split-campo to tl_sperio-tipo.

              if tl_ska1-saknr ne '0000244000'.

                if wl_valor_f gt 0
                or wl_valor_c gt 0
                or wl_valor_d gt 0
                or wl_valor_i gt 0.
                  append t_imp to t_out.
                endif.
              endif.
              append tl_sperio.
              clear: tl_sperio.

****> Flag q indica se teve algum movimento para o periodo.
              wl_155_flag = 'X'.
            endloop.

******************************************************************
            loop at tl_ska1_24000.
              clear: wl_valor_i, wl_valor_d, wl_valor_c, wl_valor_f.
****> conta de encerramento 244000
              loop at tl_faglflext_155_24000
                where racct eq tl_ska1_24000-saknr.
                clear : tl_sperio_e.

                case 'X'.
                  when p_anual.
                    add tl_faglflext_155_24000-hslvt to wl_valor_i.
                    if tl_faglflext_155_24000-drcrk eq 'S'.
                      add tl_faglflext_155_24000-hsl01 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl02 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl03 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl04 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl05 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl06 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl07 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl08 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl09 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl10 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl11 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl12 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl13 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl14 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl15 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl16 to wl_valor_d.
                    elseif tl_faglflext_155_24000-drcrk eq 'H'.
                      add tl_faglflext_155_24000-hsl01 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl02 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl03 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl04 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl05 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl06 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl07 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl08 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl09 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl10 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl11 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl12 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl13 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl14 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl15 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl16 to wl_valor_c.
                    endif.
                  when p_trim.
                    read table t_out index wl_150_l.
                    split t_out at '|' into table tl_split_aux.
                    delete tl_split_aux index 1.
                    read table tl_split_aux index 2.

******> Primeiro Trimeste.
                    if tl_split_aux-campo+2(2) eq '01'
                    or tl_split_aux-campo+2(2) eq '02'
                    or tl_split_aux-campo+2(2) eq '03'.
*******> Pega valor inicial da conta
*                      add tl_faglflext_155_24000-hs  lvt to wl_valor_i.
                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl01 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl02 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl03 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl01 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl02 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl03 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '1'.
******> Segundo Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '04'
                       or  tl_split_aux-campo+2(2) eq '05'
                       or  tl_split_aux-campo+2(2) eq '06'.
                      read table tl_sperio_e
                        with key saknr = tl_faglflext_155_24000-racct
                                 perio = '1'.
                      if sy-subrc is initial.
                        move: tl_sperio_e-sfinal to wl_valor_i.
                        if tl_sperio_e-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.
                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl04 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl05 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl06 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl04 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl05 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl06 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '2'.
******> Terceiro Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '07'
                       or  tl_split_aux-campo+2(2) eq '08'
                       or  tl_split_aux-campo+2(2) eq '09'.
                      read table tl_sperio_e
                         with key saknr = tl_faglflext_155_24000-racct
                                  perio = '2'.
                      if sy-subrc is initial.
                        move: tl_sperio_e-sfinal to wl_valor_i.
                        if tl_sperio_e-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl07 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl08 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl09 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl07 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl08 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl09 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '3'.
******> Quarto Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '10'
                       or  tl_split_aux-campo+2(2) eq '11'
                       or  tl_split_aux-campo+2(2) eq '12'.
                      read table tl_sperio_e
                        with key saknr = tl_faglflext_155_24000-racct
                                 perio = '3'.
                      if sy-subrc is initial.
                        move: tl_sperio_e-sfinal to wl_valor_i.
                        if tl_sperio_e-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl10 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl11 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl12 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl13 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl14 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl15 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl10 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl11 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl12 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl13 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl14 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl15 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '4'.
                    endif.
                endcase.
              endloop.
              if sy-subrc is not initial.
                continue.
              endif.

****> Posição I155 -> tipo de valor inicial> bloco 5
              if wl_valor_i ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 5.

****> Posição I155 -> Valor de Debito> bloco 6
              if wl_valor_d lt 0.
                multiply wl_valor_d by -1.
              endif.
              move wl_valor_d to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 6.

****> Posição I155 -> Valor de Credito> bloco 7
              if wl_valor_c lt 0.
                multiply wl_valor_c by -1.
              endif.
              move wl_valor_c to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 7.
****> Posição I155 -> Valor final calculo
              wl_valor_f = wl_valor_i + wl_valor_d - wl_valor_c.

****> Posição I155 -> Valor inicial da conta> bloco 4
              if wl_valor_i lt 0.
                multiply wl_valor_i by -1.
              endif.
              move wl_valor_i to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 4.

****> Posição I155 -> tipo de valor final> bloco 9
              if wl_valor_f ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              if wl_valor_f lt 0.
                multiply wl_valor_f by -1.
              endif.
              move wl_valor_f to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 8.

              move tl_ska1_24000-saknr to t_split-campo.
              modify t_split index 2.


              move: wl_valor_f    to tl_sperio_e-sfinal,
                    wl_valor_c    to tl_sperio_e-cperio,
                    wl_valor_d    to tl_sperio_e-dperio,
                    tl_ska1_24000-saknr to tl_sperio_e-saknr.
              read table t_split index 9.
              move t_split-campo to tl_sperio_e-tipo.

              append tl_sperio_e.
              clear: tl_sperio_e.
            endloop.
******************************************************************

****> conta de encerramento 244000
            if p_trim is not initial.
              loop at tl_faglflext_155_24000.
                clear : tl_sperio.

                case 'X'.
                  when p_anual.
                    add tl_faglflext_155_24000-hslvt to wl_valor_i.
                    if tl_faglflext_155_24000-drcrk eq 'S'.
                      add tl_faglflext_155_24000-hsl01 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl02 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl03 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl04 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl05 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl06 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl07 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl08 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl09 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl10 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl11 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl12 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl13 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl14 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl15 to wl_valor_d.
                      add tl_faglflext_155_24000-hsl16 to wl_valor_d.
                    elseif tl_faglflext_155_24000-drcrk eq 'H'.
                      add tl_faglflext_155_24000-hsl01 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl02 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl03 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl04 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl05 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl06 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl07 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl08 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl09 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl10 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl11 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl12 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl13 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl14 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl15 to wl_valor_c.
                      add tl_faglflext_155_24000-hsl16 to wl_valor_c.
                    endif.
                  when p_trim.
                    read table t_out index wl_150_l.
                    split t_out at '|' into table tl_split_aux.
                    delete tl_split_aux index 1.
                    read table tl_split_aux index 2.

******> Primeiro Trimeste.
                    if tl_split_aux-campo+2(2) eq '01'
                    or tl_split_aux-campo+2(2) eq '02'
                    or tl_split_aux-campo+2(2) eq '03'.
*******> Pega valor inicial da conta
                      add tl_faglflext_155_24000-hslvt to wl_valor_i.
                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl01 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl02 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl03 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl01 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl02 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl03 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '1'.
******> Segundo Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '04'
                       or  tl_split_aux-campo+2(2) eq '05'
                       or  tl_split_aux-campo+2(2) eq '06'.
                      read table tl_sperio
                        with key saknr = '0000244000'
                                 perio = '1'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.
                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl04 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl05 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl06 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl04 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl05 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl06 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '2'.
******> Terceiro Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '07'
                       or  tl_split_aux-campo+2(2) eq '08'
                       or  tl_split_aux-campo+2(2) eq '09'.
                      read table tl_sperio
                         with key saknr = '0000244000'
                                  perio = '2'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl07 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl08 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl09 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl07 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl08 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl09 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '3'.
******> Quarto Trimeste.
                    elseif tl_split_aux-campo+2(2) eq '10'
                       or  tl_split_aux-campo+2(2) eq '11'
                       or  tl_split_aux-campo+2(2) eq '12'.
                      read table tl_sperio
                        with key saknr = '0000244000'
                                 perio = '3'.
                      if sy-subrc is initial.
                        move: tl_sperio-sfinal to wl_valor_i.
                        if tl_sperio-tipo eq 'C'.
                          multiply wl_valor_i by -1.
                        endif.
                      endif.

                      if tl_faglflext_155_24000-drcrk eq 'S'.
                        add tl_faglflext_155_24000-hsl10 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl11 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl12 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl13 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl14 to wl_valor_d.
                        add tl_faglflext_155_24000-hsl15 to wl_valor_d.
                      elseif tl_faglflext_155_24000-drcrk eq 'H'.
                        add tl_faglflext_155_24000-hsl10 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl11 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl12 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl13 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl14 to wl_valor_c.
                        add tl_faglflext_155_24000-hsl15 to wl_valor_c.
                      endif.
                      tl_sperio_e-perio = '4'.
                    endif.
                endcase.
              endloop.

*****> Encerramento

              read table tl_sperio
                with key saknr = '0000244000'
                         perio = tl_sperio_e-perio.
              move tl_sperio-sfinal to wl_valor_i.
              if tl_sperio-tipo eq 'C'.
                multiply wl_valor_i by -1.
              endif.
****> Posição I155 -> tipo de valor inicial> bloco 5
              if wl_valor_i ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 5.


****> Posição I155 -> Valor de Debito> bloco 6
              if wl_valor_d lt 0.
                multiply wl_valor_d by -1.
              endif.
              move wl_valor_d to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 6.

****> Posição I155 -> Valor de Credito> bloco 7
              if wl_valor_c lt 0.
                multiply wl_valor_c by -1.
              endif.
              move wl_valor_c to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 7.
****> Posição I155 -> Valor final calculo
              wl_valor_f = wl_valor_i + wl_valor_d - wl_valor_c.

****> Posição I155 -> Valor inicial da conta> bloco 4
              if wl_valor_i lt 0.
                multiply wl_valor_i by -1.
              endif.
              move wl_valor_i to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 4.

****> Posição I155 -> tipo de valor final> bloco 9
              if wl_valor_f ge 0.
                move 'D' to t_split-campo.
              else.
                move 'C' to t_split-campo.
              endif.
              modify t_split index 9.

****> Posição I155 -> Valor final da conta> Bloco 8
              if wl_valor_f lt 0.
                multiply wl_valor_f by -1.
              endif.
              move wl_valor_f to t_split-campo.
              translate t_split-campo using '.,'.
              modify t_split index 8.


              move '0000244000' to t_split-campo.
              modify t_split index 2.

              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field .
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              move: wl_valor_f    to tl_sperio-sfinal,
                    wl_valor_c    to tl_sperio-cperio,
                    wl_valor_d    to tl_sperio-dperio,
                    '0000244000'  to tl_sperio-saknr.
              add 1 to tl_sperio-perio.
              read table t_split index 9.
              move t_split-campo to tl_sperio-tipo.

              if wl_valor_f gt 0
                or wl_valor_c gt 0
                or wl_valor_d gt 0
                or wl_valor_i gt 0.
                append t_imp to t_out.
              endif.
              append tl_sperio.
              clear: tl_sperio.
              if tl_faglflext_155_24000[] is not initial.
****> Flag q indica se teve algum movimento para o periodo.
                wl_155_flag = 'X'.
              endif.
              add 1 to wl_tabix.
              read table t_imp index wl_tabix.
              if t_imp-field+1(4) ne 'I150'
              and t_imp-field+1(4) ne 'I155'.
****> Caso nao haja nenhum movimento para o periodo o perido é eliminado do arquivo..
                if wl_155_flag is initial.
                  delete t_out index wl_150_l.
                  clear : wl_150_l.
                endif.
              endif.
              subtract 1 from wl_tabix.
            endif.
          endif.
        elseif t_split-campo eq 'I200'.
          if t_imp_i200[] is initial.
****> Posição 5 - POR UM 'X'
            clear t_split.
            read table t_split index 5.
            if t_split-campo ne 'E'.
              move 'X|' to t_split-campo.
              modify t_split index 5.
              loop at t_split.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
                endif.
              endloop.
              append t_imp to t_out.
              clear t_out.
              wl_200_l = sy-tabix.
              refresh tl_lancamentos.
            else.
*            APPEND T_IMP TO T_OUT.
*            WL_200_L = SY-TABIX.
              clear wl_200_l.
              refresh tl_lancamentos.
            endif.
          else.
            loop at t_imp_i200 into t_imp.
              wl_tabix = sy-tabix.
              split t_imp at '|' into table t_split.
              delete t_split index 1.
              if t_split[] is not initial.
                read table t_split index 1.
                if t_split-campo eq 'I200'.
****> posição 5 - por um 'X'
                  clear t_split.
                  read table t_split index 5.
                  if t_split-campo ne 'E'.
                    move 'X|' to t_split-campo.
                    modify t_split index 5.
                    loop at t_split.
                      if sy-tabix eq 1.
                        concatenate '|'  t_split-campo into t_imp-field.
                      else.
                        concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
                      endif.
                    endloop.
                    append t_imp to t_out.
                    clear t_out.
                    wl_200_l = sy-tabix.
                    refresh tl_lancamentos.
                  else.
*            APPEND T_IMP TO T_OUT.
*            WL_200_L = SY-TABIX.
                    clear wl_200_l.
                    refresh tl_lancamentos.
                  endif.

                elseif t_split-campo eq 'I250'.
                  if wl_200_l is not initial.
*            READ TABLE T_OUT INDEX WL_200_L.
*            SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
*            DELETE TL_SPLIT_AUX INDEX 1.
*            READ TABLE TL_SPLIT_AUX INDEX 5.
*            IF TL_SPLIT_AUX-CAMPO NE 'E'.
                    read table t_split index 2.

                    move t_imp-field to tl_lancamentos-field.
*            read table tl_ska1
*              with key saknr = t_split-campo.
                    if t_split-campo in s_hkont.

                      move 'X' to tl_lancamentos-flag.
                    else.

                      move space to tl_lancamentos-flag.
                    endif.
                    append tl_lancamentos.
                    add 1 to wl_tabix.
                    clear: tl_imp_aux.
                    read table t_imp_i200 into tl_imp_aux index wl_tabix.
                    if tl_imp_aux-field+1(4) ne 'I250'.
                      read table tl_lancamentos
                        with key flag = 'X'.
                      if sy-subrc is initial.
                        loop at tl_lancamentos.
                          move tl_lancamentos-field to t_out-field.

                          append t_out.
                          clear: t_out.
                        endloop.
*
*                READ TABLE TL_LANCAMENTOS
*                WITH KEY FLAG = SPACE.
*                IF SY-SUBRC IS INITIAL.
                        read table t_out into t_imp index wl_200_l.
                        split t_imp at '|' into table t_split.
                        delete t_split index 1.

                        t_split-campo = 'EF|'.
                        modify t_split index 5.
                        clear: t_out.
                        loop at t_split.
                          if sy-tabix eq 1.
                            concatenate '|'  t_split-campo into t_imp-field.
                          else.
                            concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
                          endif.
                        endloop.
                        append t_imp to t_out.
                        clear: t_out.

                        loop at tl_lancamentos.
                          if tl_lancamentos-flag is initial.
                            read table tl_ska1
                              with key saknr = tl_lancamentos+6(10).
                            if sy-subrc is initial.
                              tl_lancamentos-field+6(10) = '0000244000'.
                            else.

                            endif.
                          endif.
                          move tl_lancamentos-field to t_out-field.

                          append t_out.
                          clear: t_out.
                        endloop.
*                ENDIF.
                      else.
                        delete t_out index wl_200_l.
                        clear wl_200_l.
                      endif.
                    endif.
                    subtract 1 from wl_tabix.
*              DELETE T_OUT INDEX WL_200_L.
*              CLEAR WL_200_L.
*              LOOP AT TL_LANCAMENTO.
*                DELETE T_OUT WHERE FIELD EQ TL_LANCAMENTO-FIELD.
*              ENDLOOP.
*            ELSE.
*              MOVE T_IMP TO T_OUT.
*              APPEND T_OUT.
*              APPEND T_OUT TO TL_LANCAMENTO.
*              CLEAR: T_OUT.
*            ENDIF.
*            ELSE.
*              APPEND T_IMP TO T_OUT.
*            ENDIF.
                  endif.
                endif.
              endif.
            endloop.
          endif.
        elseif t_split-campo eq 'I250'.
          if t_imp_i200[] is initial.
            if wl_200_l is not initial.
*            READ TABLE T_OUT INDEX WL_200_L.
*            SPLIT T_OUT AT '|' INTO TABLE TL_SPLIT_AUX.
*            DELETE TL_SPLIT_AUX INDEX 1.
*            READ TABLE TL_SPLIT_AUX INDEX 5.
*            IF TL_SPLIT_AUX-CAMPO NE 'E'.
              read table t_split index 2.

              move t_imp-field to tl_lancamentos-field.
*            read table tl_ska1
*              with key saknr = t_split-campo.
              if t_split-campo in s_hkont.

                move 'X' to tl_lancamentos-flag.
              else.

                move space to tl_lancamentos-flag.
              endif.
              append tl_lancamentos.
              add 1 to wl_tabix.
              read table t_imp into tl_imp_aux index wl_tabix.
              if tl_imp_aux-field+1(4) ne 'I250'.
                read table tl_lancamentos
                  with key flag = 'X'.
                if sy-subrc is initial.
                  loop at tl_lancamentos.
                    move tl_lancamentos-field to t_out-field.

                    append t_out.
                    clear: t_out.
                  endloop.
*
*                READ TABLE TL_LANCAMENTOS
*                WITH KEY FLAG = SPACE.
*                IF SY-SUBRC IS INITIAL.
                  read table t_out into t_imp index wl_200_l.
                  split t_imp at '|' into table t_split.
                  delete t_split index 1.

                  t_split-campo = 'EF|'.
                  modify t_split index 5.
                  clear: t_out.
                  loop at t_split.
                    if sy-tabix eq 1.
                      concatenate '|'  t_split-campo into t_imp-field.
                    else.
                      concatenate t_imp-field t_split-campo into t_imp-field separated by '|'.
                    endif.
                  endloop.
                  append t_imp to t_out.
                  clear: t_out.

                  loop at tl_lancamentos.
                    if tl_lancamentos-flag is initial.
                      read table tl_ska1
                        with key saknr = tl_lancamentos+6(10).
                      if sy-subrc is initial.
                        tl_lancamentos-field+6(10) = '0000244000'.
                      else.

                      endif.
                    endif.
                    move tl_lancamentos-field to t_out-field.

                    append t_out.
                    clear: t_out.
                  endloop.
*                ENDIF.
                else.
                  delete t_out index wl_200_l.
                  clear wl_200_l.
                endif.
              endif.
              subtract 1 from wl_tabix.
*              DELETE T_OUT INDEX WL_200_L.
*              CLEAR WL_200_L.
*              LOOP AT TL_LANCAMENTO.
*                DELETE T_OUT WHERE FIELD EQ TL_LANCAMENTO-FIELD.
*              ENDLOOP.
*            ELSE.
*              MOVE T_IMP TO T_OUT.
*              APPEND T_OUT.
*              APPEND T_OUT TO TL_LANCAMENTO.
*              CLEAR: T_OUT.
*            ENDIF.
*            ELSE.
*              APPEND T_IMP TO T_OUT.
*            ENDIF.
            endif.
          endif.
        elseif t_split-campo eq 'I350'.
          case 'X'.
            when p_anual.
              concatenate '3112' s_gjahr-low into t_split-campo.
              modify t_split index 2.
              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              append t_imp to t_out.
              clear: t_imp.
            when p_trim.
****** Primeiro Trimeste
              concatenate s_gjahr-low '03' '01' into wl_data_aux.
              call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
                exporting
                  day_in            = wl_data_aux
                importing
                  last_day_of_month = wl_data_aux2
                exceptions
                  day_in_not_valid  = 1
                  others            = 2.

              concatenate wl_data_aux2+6(2) wl_data_aux2+4(2) wl_data_aux2(4) into t_split-campo.
              modify t_split index 2.
              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              append t_imp to t_out.

*********> Posição I355
              clear: wl_valor_i.
              loop at tl_faglflext_155_24000.
                read table tl_sperio_e
                  with key saknr = tl_faglflext_155_24000-racct
                           perio = '1'.
                if sy-subrc is initial.
                  read table t_contas
                    with key saknr = tl_faglflext_155_24000-racct
                             perio = '1'.
                  if sy-subrc is not initial.
                    wl_valor_i = tl_sperio_e-dperio - tl_sperio_e-cperio.
                    move: wl_valor_i to wl_valor_aux.
                    translate wl_valor_aux using '.,'.
                    condense wl_valor_aux no-gaps.
                    if wl_valor_i ge 0.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|C|' into t_out-field.
                    else.
                      multiply wl_valor_i by -1.
                      move: wl_valor_i to wl_valor_aux.
                      translate wl_valor_aux using '.,'.
                      condense wl_valor_aux no-gaps.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|D|' into t_out-field.
                    endif.
                    append t_out.
                    move : tl_sperio_e-saknr to t_contas-saknr,
                           '1'               to t_contas-perio.
                    append t_contas.
                  endif.
                endif.
                clear: t_out, wl_valor_i, wl_valor_aux, tl_sperio_e, t_contas.
              endloop.

****** Segundo Trimeste
              concatenate s_gjahr-low '06' '01' into wl_data_aux.
              call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
                exporting
                  day_in            = wl_data_aux
                importing
                  last_day_of_month = wl_data_aux2
                exceptions
                  day_in_not_valid  = 1
                  others            = 2.

              concatenate wl_data_aux2+6(2) wl_data_aux2+4(2) wl_data_aux2(4) into t_split-campo.
              modify t_split index 2.
              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              append t_imp to t_out.

*********> Posição I355
              clear: wl_valor_i.
              loop at tl_faglflext_155_24000.
                read table tl_sperio_e
                  with key saknr = tl_faglflext_155_24000-racct
                           perio = '2'.
                if sy-subrc is initial.
                  read table t_contas
                    with key saknr = tl_faglflext_155_24000-racct
                             perio = '2'.
                  if sy-subrc is not initial.
                    wl_valor_i = tl_sperio_e-dperio - tl_sperio_e-cperio.
                    move: wl_valor_i to wl_valor_aux.
                    translate wl_valor_aux using '.,'.
                    condense wl_valor_aux no-gaps.
                    if wl_valor_i ge 0.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|C|' into t_out-field.
                    else.

                      multiply wl_valor_i by -1.
                      move: wl_valor_i to wl_valor_aux.
                      translate wl_valor_aux using '.,'.
                      condense wl_valor_aux no-gaps.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|D|' into t_out-field.
                    endif.
                    append t_out.
                    move : tl_sperio_e-saknr to t_contas-saknr,
                           '2'               to t_contas-perio.
                    append t_contas.
                  endif.
                endif.
                clear: t_out, wl_valor_i, wl_valor_aux, tl_sperio_e, t_contas.
              endloop.

****** Terceiro Trimeste
              concatenate s_gjahr-low '09' '01' into wl_data_aux.
              call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
                exporting
                  day_in            = wl_data_aux
                importing
                  last_day_of_month = wl_data_aux2
                exceptions
                  day_in_not_valid  = 1
                  others            = 2.

              concatenate wl_data_aux2+6(2) wl_data_aux2+4(2) wl_data_aux2(4) into t_split-campo.
              modify t_split index 2.
              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              append t_imp to t_out.
*********> Posição I355
              clear: wl_valor_i.
              loop at tl_faglflext_155_24000.
                read table tl_sperio_e
                  with key saknr = tl_faglflext_155_24000-racct
                           perio = '3'.
                if sy-subrc is initial.
                  read table t_contas
                    with key saknr = tl_faglflext_155_24000-racct
                             perio = '3'.
                  if sy-subrc is not initial.
                    wl_valor_i = tl_sperio_e-dperio - tl_sperio_e-cperio.
                    move: wl_valor_i to wl_valor_aux.
                    translate wl_valor_aux using '.,'.
                    condense wl_valor_aux no-gaps.
                    if wl_valor_i ge 0.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|C|' into t_out-field.
                    else.
                      multiply wl_valor_i by -1.
                      move: wl_valor_i to wl_valor_aux.
                      translate wl_valor_aux using '.,'.
                      condense wl_valor_aux no-gaps.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|D|' into t_out-field.
                    endif.
                    append t_out.
                    move : tl_sperio_e-saknr to t_contas-saknr,
                           '3'               to t_contas-perio.
                    append t_contas.
                  endif.
                endif.
                clear: t_out, wl_valor_i, wl_valor_aux, tl_sperio_e, t_contas.
              endloop.

****** Quarto Trimeste
              concatenate s_gjahr-low '12' '01' into wl_data_aux.
              call function 'SG_PS_GET_LAST_DAY_OF_MONTH'
                exporting
                  day_in            = wl_data_aux
                importing
                  last_day_of_month = wl_data_aux2
                exceptions
                  day_in_not_valid  = 1
                  others            = 2.

              concatenate wl_data_aux2+6(2) wl_data_aux2+4(2) wl_data_aux2(4) into t_split-campo.
              modify t_split index 2.
              loop at t_split.
                condense t_split-campo no-gaps.
                if sy-tabix eq 1.
                  concatenate '|'  t_split-campo into t_imp-field.
                else.
                  concatenate t_imp-field '|' t_split-campo into t_imp-field.
                endif.
                at last.
                  concatenate t_imp-field '|' into t_imp-field.
                endat.
              endloop.
              append t_imp to t_out.
*********> Posição I355
              clear: wl_valor_i.
              loop at tl_faglflext_155_24000.
                read table tl_sperio_e
                  with key saknr = tl_faglflext_155_24000-racct
                           perio = '4'.
                if sy-subrc is initial.
                  read table t_contas
                    with key saknr = tl_faglflext_155_24000-racct
                             perio = '4'.
                  if sy-subrc is not initial.
                    wl_valor_i = tl_sperio_e-dperio - tl_sperio_e-cperio.
                    move: wl_valor_i to wl_valor_aux.
                    translate wl_valor_aux using '.,'.
                    condense wl_valor_aux no-gaps.
                    if wl_valor_i ge 0.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|C|' into t_out-field.
                    else.
                      multiply wl_valor_i by -1.
                      move: wl_valor_i to wl_valor_aux.
                      translate wl_valor_aux using '.,'.
                      condense wl_valor_aux no-gaps.
                      concatenate '|I355|' tl_sperio_e-saknr '||' wl_valor_aux '|D|' into t_out-field.
                    endif.
                    append t_out.
                    move : tl_sperio_e-saknr to t_contas-saknr,
                           '4'               to t_contas-perio.
                    append t_contas.
                  endif.
                endif.
                clear: t_out, wl_valor_i, wl_valor_aux, tl_sperio_e, t_contas.
              endloop.

          endcase.

        elseif t_split-campo eq 'I355'.
          if p_anual is not initial.
            append t_imp to t_out.
****> Pega contas para selecionar em tabela FAGLFLEXT
            clear: t_split, t_contas.
            read table t_split index 2.
            if t_split-campo is not  initial.
              move: t_split-campo to t_contas-saknr.
              append t_contas.
              clear t_contas.
            endif.
          endif.

        elseif t_split-campo eq 'J990'.
          append t_imp to t_out.
****> Posição M001
          clear: t_out.
          move: '|M001|0|' to t_out-field.
          append t_out.
****> Posição M020
          case 'X'.
            when p_anual.
******> Anual
              clear: t_out.
*              move: '|M020|10|0||||0|A00|1|||' to t_out-field.
              move: '|M020|10|0||||0|A|1|||' to t_out-field.
              append t_out.
            when p_trim.
******> 1º Trimestre
              clear: t_out.
*              move: '|M020|10|0||||0|T01|1|||' to t_out-field.
              move: '|M020|10|0||||0|T|1|||' to t_out-field.
              append t_out.
*******> 2º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T02|1|||' to t_out-field.
*              append t_out.
*******> 3º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T03|1|||' to t_out-field.
*              append t_out.
*******> 4º Trimestre
*              clear: t_out.
*              move: '|M020|10|0||||0|T04|1|||' to t_out-field.
*              append t_out.
          endcase.

****> Posição M030
          if t_faglflext[] is initial.
            select racct drcrk hslvt
                   hsl01 hsl02 hsl03 hsl04 hsl05
                   hsl06 hsl07 hsl08 hsl09 hsl10
                   hsl11 hsl12 hsl13 hsl14 hsl15 hsl16
              from faglflext
              into table t_faglflext
               for all entries in t_contas
               where ryear in s_gjahr
                 and rtcur eq 'BRL'
                 and rldnr eq '0L'
                 and racct eq t_contas-saknr
                 and rbukrs in s_bukrs.

          endif.
          case 'X'.
            when p_anual.
              clear: wl_valor_a.
              loop at t_faglflext.
                add t_faglflext-hsl01 to wl_valor_a.
                add t_faglflext-hsl02 to wl_valor_a.
                add t_faglflext-hsl03 to wl_valor_a.
                add t_faglflext-hsl04 to wl_valor_a.
                add t_faglflext-hsl05 to wl_valor_a.
                add t_faglflext-hsl06 to wl_valor_a.
                add t_faglflext-hsl07 to wl_valor_a.
                add t_faglflext-hsl08 to wl_valor_a.
                add t_faglflext-hsl09 to wl_valor_a.
                add t_faglflext-hsl10 to wl_valor_a.
                add t_faglflext-hsl11 to wl_valor_a.
                add t_faglflext-hsl12 to wl_valor_a.
                add t_faglflext-hsl13 to wl_valor_a.
                add t_faglflext-hsl14 to wl_valor_a.
                add t_faglflext-hsl15 to wl_valor_a.
                add t_faglflext-hsl16 to wl_valor_a.

              endloop.
******> Anual
              clear: t_out, wl_valor_aux, wl_valor_a.
              move wl_valor_a to wl_valor_aux.
              translate wl_valor_aux using ', '.
              translate wl_valor_aux using '.,'.
              condense wl_valor_aux no-gaps.
              if wl_valor_a ge 0.
                concatenate '|M030|A00|' wl_valor_aux '|D|' into t_out-field.
              else.
                multiply wl_valor_a by -1.
                move wl_valor_a to wl_valor_aux.
                translate wl_valor_aux using ', '.
                translate wl_valor_aux using '.,'.
                condense wl_valor_aux no-gaps.
                concatenate '|M030|A00|' wl_valor_aux '|C|' into t_out-field.
              endif.
              append t_out.
            when p_trim.
******> 1º Trimestre
              clear: wl_valor_1.
              loop at t_faglflext.
                add t_faglflext-hsl01 to wl_valor_1.
                add t_faglflext-hsl02 to wl_valor_1.
                add t_faglflext-hsl03 to wl_valor_1.

              endloop.
              clear: t_out, wl_valor_aux, wl_valor_1.
              move wl_valor_1 to wl_valor_aux.
              translate wl_valor_aux using ', '.
              translate wl_valor_aux using '.,'.
              condense wl_valor_aux no-gaps.

              if wl_valor_1 ge 0.
                concatenate '|M030|T01|' wl_valor_aux '|D|' into t_out-field.
              else.
                multiply wl_valor_1 by -1.
                move wl_valor_a to wl_valor_aux.
                translate wl_valor_aux using ', '.
                translate wl_valor_aux using '.,'.
                condense wl_valor_aux no-gaps.
                concatenate '|M030|T01|' wl_valor_aux '|C|' into t_out-field.
              endif.
              append t_out.
******> 2º Trimestre
              clear: wl_valor_2.
              loop at t_faglflext.
                add t_faglflext-hsl04 to wl_valor_2.
                add t_faglflext-hsl05 to wl_valor_2.
                add t_faglflext-hsl06 to wl_valor_2.

              endloop.
              clear: t_out, wl_valor_aux, wl_valor_2.
              move wl_valor_2 to wl_valor_aux.
              translate wl_valor_aux using ', '.
              translate wl_valor_aux using '.,'.
              condense wl_valor_aux no-gaps.

              if wl_valor_2 ge 0.
                concatenate '|M030|T02|' wl_valor_aux '|D|' into t_out-field.
              else.
                multiply wl_valor_2 by -1.
                move wl_valor_a to wl_valor_aux.
                translate wl_valor_aux using ', '.
                translate wl_valor_aux using '.,'.
                condense wl_valor_aux no-gaps.
                concatenate '|M030|T02|' wl_valor_aux '|C|' into t_out-field.
              endif.
              append t_out.
******> 3º Trimestre
              clear: wl_valor_3.
              loop at t_faglflext.
                add t_faglflext-hsl07 to wl_valor_3.
                add t_faglflext-hsl08 to wl_valor_3.
                add t_faglflext-hsl09 to wl_valor_3.

              endloop.
              clear: t_out, wl_valor_aux, wl_valor_3.
              move wl_valor_3 to wl_valor_aux.
              translate wl_valor_aux using ', '.
              translate wl_valor_aux using '.,'.
              condense wl_valor_aux no-gaps.
              if wl_valor_3 ge 0.
                concatenate '|M030|T03|' wl_valor_aux '|D|' into t_out-field.
              else.
                multiply wl_valor_3 by -1.
                move wl_valor_a to wl_valor_aux.
                translate wl_valor_aux using ', '.
                translate wl_valor_aux using '.,'.
                condense wl_valor_aux no-gaps.
                concatenate '|M030|T03|' wl_valor_aux '|C|' into t_out-field.
              endif.
              append t_out.
******> 4º Trimestre
              clear: wl_valor_4.
              loop at t_faglflext.
                add t_faglflext-hsl10 to wl_valor_4.
                add t_faglflext-hsl11 to wl_valor_4.
                add t_faglflext-hsl12 to wl_valor_4.
                add t_faglflext-hsl13 to wl_valor_4.
                add t_faglflext-hsl14 to wl_valor_4.
                add t_faglflext-hsl15 to wl_valor_4.

              endloop.
              clear: t_out, wl_valor_aux, wl_valor_4.
              move wl_valor_4 to wl_valor_aux.
              translate wl_valor_aux using ', '.
              translate wl_valor_aux using '.,'.
              condense wl_valor_aux no-gaps.

              if wl_valor_4 ge 0.
                concatenate '|M030|T04|' wl_valor_aux '|D|' into t_out-field.
              else.
                multiply wl_valor_4 by -1.
                move wl_valor_a to wl_valor_aux.
                translate wl_valor_aux using ', '.
                translate wl_valor_aux using '.,'.
                condense wl_valor_aux no-gaps.
                concatenate '|M030|T04|' wl_valor_aux '|C|' into t_out-field.
              endif.
              append t_out.
          endcase.

****> Posição M990
          clear: t_out.
          move: '|M990|0|' to t_out-field.
          append t_out.

****> Posição 9001
          clear: t_out.
          move: '|9001|0|' to t_out-field.
          append t_out.

****> Posição 9990
          clear: t_out.
          move: '|9990|0|' to t_out-field.
          append t_out.
****> Posição 9900
        elseif t_split-campo eq '9900'
            or t_split-campo eq '9001'
            or t_split-campo eq '9990'
            or t_split-campo eq '9999'.
*****> Não faz nada
        else.
          append t_imp to t_out.
        endif.
      endif.
    endif.
  endloop.

***> Posição M990
  refresh: tl_out_aux, t_split.
  clear: wl_linhas.
  tl_out_aux[] = t_out[].

  delete tl_out_aux where field+1(1) ne 'M'.
  delete tl_out_aux where field+1(4) eq 'M990'.
  describe table tl_out_aux lines wl_linhas.
  read table t_out
   with key field+1(4) = 'M990'.
  if sy-subrc is initial.
    wl_tabix = sy-tabix.
    split t_out at '|' into table t_split.
    delete t_split index 1.
    if t_split[] is not initial.
      read table t_split index 1.
      wl_linhas_aux = wl_linhas + 1.
      condense wl_linhas_aux no-gaps.
      concatenate '|' t_split-campo '|' wl_linhas_aux '|' into t_out-field.

      modify t_out index wl_tabix.
    endif.
  endif.

***> Posição 9900
  clear: wl_cont, wl_cont.
  loop at t_out.
    read table t_out transporting no fields
      with key field+1(4)  = '9900'
               field+6(4)  = t_out-field+1(4).
    if sy-subrc is not initial.
      if t_out-field+1(4) ne '9900'
     and t_out-field+1(4) ne '9990'.
        add 1 to wl_cont.
        tl_out_aux[] = t_out[].
        delete tl_out_aux where field+1(4) ne t_out-field+1(4).
        describe table tl_out_aux lines wl_linhas.
        wl_linhas_aux = wl_linhas.
        condense wl_linhas_aux no-gaps.
        concatenate '|9900|' t_out-field+1(4) '|' wl_linhas_aux '|' into t_out-field.
        read table t_out transporting no fields
          with key field+1(4) = '9001'.
        if sy-subrc is initial.
          wl_tabix = sy-tabix + wl_cont.
          insert t_out index wl_tabix.
        endif.
      endif.
    endif.
  endloop.

****> Posição 9900 -> 9900
  wl_linhas_aux = wl_cont + 3.
  condense wl_linhas_aux no-gaps.
  concatenate '|9900|9900|' wl_linhas_aux '|' into t_out-field.
  read table t_out transporting no fields
    with key field+1(4) = '9001'.
  if sy-subrc is initial.
    add 1 to wl_cont.
    wl_tabix = sy-tabix + wl_cont.
    insert t_out index wl_tabix.
  endif.

****> Posição 9900 -> 9990
  move: '|9900|9990|1|' to t_out-field.
  read table t_out transporting no fields
    with key field+1(4) = '9001'.
  if sy-subrc is initial.
    add 1 to wl_cont.
    wl_tabix = sy-tabix + wl_cont.
    insert t_out index wl_tabix.
  endif.

****> Posição 9900 -> 9999
  move: '|9900|9999|1|' to t_out-field.
  read table t_out transporting no fields
    with key field+1(4) = '9001'.
  if sy-subrc is initial.
    add 1 to wl_cont.
    wl_tabix = sy-tabix + wl_cont.
    insert t_out index wl_tabix.
  endif.

***> Posição 9990
  refresh: tl_out_aux, t_split.
  clear: wl_linhas.
  tl_out_aux[] = t_out[].

  delete tl_out_aux where field+1(1) ne '9'.
  describe table tl_out_aux lines wl_linhas.
  read table t_out
   with key field+1(4) = '9990'.
  if sy-subrc is initial.
    wl_tabix = sy-tabix.
    split t_out at '|' into table t_split.
    delete t_split index 1.
    if t_split[] is not initial.
      read table t_split index 1.
      wl_linhas_aux = wl_linhas + 1.
      condense wl_linhas_aux no-gaps.
      concatenate '|' t_split-campo '|' wl_linhas_aux '|' into t_out-field.

      modify t_out index wl_tabix.
    endif.
  endif.

***> Posição 9999
  describe table t_out lines wl_linhas.
  wl_linhas_aux = wl_linhas + 1.
  condense wl_linhas_aux no-gaps.
  concatenate '|9999|' wl_linhas_aux '|' into t_out-field.
  append t_out.
endform.                    " CONVERTE_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  EXPORTA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exporta_arquivo .

  if t_out[] is not initial.
    x_filename = p_f_out.
    call function 'GUI_DOWNLOAD'
      exporting
        filename = x_filename
      tables
        data_tab = t_out
      exceptions
        others   = 22.
    if sy-subrc <> 0.
    endif.
  endif.
endform.                    " EXPORTA_ARQUIVO
*&---------------------------------------------------------------------*
*&      Form  LER_TXT_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ler_txt_unix tables tl_imp
                  using p_file.

*  x_filename = p_f_in.
  x_filename = p_file.
  perform abrir_arquivo_unix using x_filename
                                   'IN'.

  do.
    read dataset x_filename into tl_imp.
    if sy-subrc = 0.
      append tl_imp.
      clear: tl_imp.
    else.
      exit.
    endif.
  enddo.
  PERFORM FECHAR_ARQUIVO_UNIX.
endform.                    " LER_TXT_UNIX
*-----------------------------------------------------------------------
* Form  FECHAR_ARQUIVO_UNIX
*-----------------------------------------------------------------------
form fechar_arquivo_unix.

  close dataset x_filename.

  if sy-subrc ne 0.
    write: / 'Problemas no close dataset coletor', sy-subrc.
  else.
*    message e836(sd) with 'Arquivo lido no UNIX'.
  endif.

endform.                    "FECHAR_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  F_MODIFICA_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_modifica_tela.
*  LOOP AT SCREEN.
*    IF SCREEN-NAME EQ 'P_BUTXT'
*    OR SCREEN-NAME EQ 'P_NAME'
*    OR SCREEN-NAME EQ 'P_NAME2'
*    OR SCREEN-NAME EQ 'P_VIVO'.
*
*if p
*      SCREEN-INPUT = 0.
**      IF SCREEN-NAME EQ 'P_VIVO'.
**        SCREEN-INVISIBLE = 1.
**      ENDIF.
*      MODIFY SCREEN.
*
*    ENDIF.
*
*  ENDLOOP.
*  if sy-ucomm eq 'ASD'.
*  case 'X'.
*    when p_unix.
*      if ok-code ne 'UNIX'.
*        move '/usr/interfaces/fcont_in/' to p_f_in.
*        move '/usr/interfaces/fcont_out/' to p_f_out.
*        ok-code = 'UNIX'.
*      endif.
*    when p_local.
*      if ok-code ne 'LOCAL'.
*        move 'C:\' to p_f_in.
*        move 'C:\' to p_f_out.
*        ok-code = 'LOCAL'.
*      endif.
*  endcase.
*  endif.
endform.                    " F_MODIFICA_TELA
*-----------------------------------------------------------------------
* Form  ABRIR_ARQUIVO_UNIX
*-----------------------------------------------------------------------
form abrir_arquivo_unix using p_file
                              wl_tYpe .
*data: wl_file_local(255) type c.
*  concatenate:  p_file  '.txt' into x_filename in character mode .    "smart: 11/01/10 E101
  move p_file to x_filename.
*wl_file_local = '/usr/interfaces/dre/TESTE4.TXT'.
  translate:    x_filename     to   lower case.
*condense wl_file_local no-gaps.
  if wl_type eq 'IN'.
  open dataset  x_filename for input in text mode message x_msgerro   "smart: 11/01/10 E111
    encoding default .                                                "smart: 11/01/10 E111
ELSE.
  open dataset  x_filename for OUTPUT in text mode message x_msgerro   "smart: 11/01/10 E111
    encoding default .
  ENDIF.
*open dataset  wl_file_local for input in text mode "message x_msgerro
*                              encoding default
*                              with smart linefeed.
  if sy-subrc ne 0.
    message e836(sd) with p_file x_msgerro sy-subrc.
  endif.

endform.                    "ABRIR_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  EXPORTA_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form exporta_arquivo_unix .

  x_filename = p_f_out.
  perform abrir_arquivo_unix using x_filename
                                   'OUT'.
  loop at t_out.
    transfer t_out to x_filename.
  endloop.

  close dataset x_filename.

  MESSAGE S836(SD) WITH 'O arquivo foi gravado com'
                         'sucesso no servidor!'.

endform.                    " EXPORTA_ARQUIVO_UNIX
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS_I200
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form seleciona_dados_i200 .
  data: tl_faglflexa type table of faglflexa with header line,
        tl_faglflexa_aux type table of faglflexa with header line,
        tl_bsis type table of bsis with header line,
        tl_bsas type table of bsas with header line,
        wl_total type faglflexa-hsl,
        wl_valor_aux(20).
  refresh: tl_faglflexa, tl_faglflexa_aux, tl_bsis, tl_bsas.

  select *
    from faglflexa
    into table tl_faglflexa
     where ryear in s_gjahr
       and rldnr eq '0L'
       and rbukrs in s_bukrs
*       and racct in s_hkont
       and poper ne '16'.

  if sy-subrc is initial.
    select *
      from bsis
      into table tl_bsis
      for all entries in tl_faglflexa
       where gjahr eq tl_faglflexa-ryear
         and bukrs eq tl_faglflexa-rbukrs
         and belnr eq tl_faglflexa-docnr
         and buzei eq tl_faglflexa-buzei.

    select *
    from bsas
    into table tl_bsas
    for all entries in tl_faglflexa
     where gjahr eq tl_faglflexa-ryear
       and bukrs eq tl_faglflexa-rbukrs
       and belnr eq tl_faglflexa-docnr
       and buzei eq tl_faglflexa-buzei.
  endif.
  tl_faglflexa_aux[] = tl_faglflexa[].
  delete tl_faglflexa_aux where drcrk ne 'S'.

  sort tl_faglflexa by docnr.
  loop at tl_faglflexa.
    on change of tl_faglflexa-docnr.
      clear: wl_total.
      loop at tl_faglflexa_aux
        where docnr eq tl_faglflexa-docnr.
        add tl_faglflexa_aux-hsl to wl_total.
      endloop.
      move wl_total to wl_valor_aux.
      translate wl_valor_aux using ', '.
      translate wl_valor_aux using '.,'.
      condense wl_valor_aux no-gaps.
      concatenate '|I200|' tl_faglflexa-docnr '|' tl_faglflexa-budat+6(2) tl_faglflexa-budat+4(2) tl_faglflexa-budat(4) '|' wl_valor_aux '|X|'
      into t_imp_i200-field.

      append t_imp_i200.
      clear: t_imp_i200.
    endon.

    if tl_faglflexa-hsl lt 0.
      multiply tl_faglflexa-hsl by -1.
    endif.
    read table tl_bsis
      with key bukrs = tl_faglflexa-rbukrs
               gjahr = tl_faglflexa-ryear
               belnr = tl_faglflexa-docnr.
    if sy-subrc is not initial.
      read table tl_bsas into tl_bsis
      with key bukrs = tl_faglflexa-rbukrs
               gjahr = tl_faglflexa-ryear
               belnr = tl_faglflexa-docnr.
    endif.
    move tl_faglflexa-hsl to wl_valor_aux.
    translate wl_valor_aux using ', '.
    translate wl_valor_aux using '.,'.
    condense wl_valor_aux no-gaps.
    if tl_faglflexa-drcrk eq 'H'.
      concatenate '|I250|' tl_faglflexa-racct '||' wl_valor_aux '|C|||' tl_bsis-sgtxt '||' into t_imp_i200-field.
    else.
      concatenate '|I250|' tl_faglflexa-racct '||' wl_valor_aux '|D|||' tl_bsis-sgtxt '||' into t_imp_i200-field.
    endif.
    append t_imp_i200.
    clear: t_imp_i200.
  endloop.
endform.                    " SELECIONA_DADOS_I200
