************************************************************************
*  A M A G G I  E X P O R T A Ç Ã O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski                                  *
* Data desenv ...: 30.05.2008                                          *
* Tipo de prg ...: Report                                              *
* Objetivo    ...: Conferencia de Fiscal X Contábil                    *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 30.05.2008    Michely              Criação              DEVK904114   *
*                                                                      *
************************************************************************

report zfis009 no standard page heading    "Não exibe cabeçalho standard
               message-id z01
               line-size 076               "Comprimento da Linha
               line-count 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
tables: rbkp.

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
type-pools: slis,
            kkblo.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
data: it_fieldcat        type slis_t_fieldcat_alv,                   "Estrutura de saida
      it_event           type slis_t_event       with header line,   "Eventos
      it_header          type kkblo_t_listheader with header line,   "Cabeçalho
      wa_fieldcat        like line of it_fieldcat,
      vg_layout          type slis_layout_alv.   "Layout do alv

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
constants: c_tabela       type c length 12 value 'IT_RELATORIO'.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_rbkp,
        belnr           like rbkp-belnr,
        gjahr           like rbkp-gjahr,
        budat           like rbkp-budat,
        stblg           like rbkp-stblg,
        usnam           like rbkp-usnam,
      end   of wa_rbkp,

      begin of wa_rseg,
        belnr           like rseg-belnr,
        ebeln           like rseg-ebeln,
        bukrs           like rseg-bukrs,
        werks           like rseg-werks,
      end   of wa_rseg,

      begin of wa_ekbe,
        ebeln           like ekbe-ebeln,
        belnr           like ekbe-belnr,
        vgabe           like ekbe-vgabe,
        matnr           like ekbe-matnr,
      end   of wa_ekbe,

      begin of wa_relatorio,
        bukrs           like rbkp-bukrs,
        werks           like rseg-werks,
        belnr           like rseg-belnr,
        ebeln           like rseg-ebeln,
        budat           like rbkp-budat,
        gjahr           like rbkp-gjahr,
        bsart           like ekko-bsart,
        usnam           like rbkp-usnam,
      end   of wa_relatorio,

      begin of wa_ekko,
        ebeln           like ekko-ebeln,
        bsart           like ekko-bsart,
      end   of wa_ekko.

data: it_rbkp           like standard table of wa_rbkp,
      it_rseg           like standard table of wa_rseg,
      it_ekbe           like standard table of wa_ekbe,
      it_relatorio      like standard table of wa_relatorio,
      it_ekko           like standard table of wa_ekko.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
data: r_gjahr            like range of rbkp-gjahr with header line. " Exemplo zfib005

*----------------------------------------------------------------------*
* Definição de Macro                                                   *
*----------------------------------------------------------------------*
* Preenche a tabela fieldcat
define fieldcat.
  clear wa_fieldcat.
  wa_fieldcat-key        = &1. "Campo chave
  wa_fieldcat-tabname    = &2. "Tabela interna
  wa_fieldcat-fieldname  = &3. "Campo da tabela interna
  wa_fieldcat-fix_column = &4. "Congelar a coluna
  wa_fieldcat-hotspot    = &5. "Cria link no campo (X)
  wa_fieldcat-seltext_s  = &6. "Descrição grande
  wa_fieldcat-seltext_m  = &7. "Descrição media
  wa_fieldcat-seltext_l  = &8. "Descrição pequena
  append wa_fieldcat to it_fieldcat.
end-of-definition.

*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s00.
parameters:
            p_bukrs    like rbkp-bukrs obligatory.
select-options:
            s_budat    for rbkp-budat obligatory.
selection-screen end   of block b0.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
initialization.
  set titlebar 'INI'.


*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  refresh r_gjahr.
  clear r_gjahr.
  r_gjahr-sign = 'I'.
  r_gjahr-option = 'BT'.
  r_gjahr-low = s_budat-low(4).
  r_gjahr-high = s_budat-high(4).
  append r_gjahr.

  perform f_seleciona_dados.
  perform f_monta_dados.
  perform f_monta_alv.
  perform f_executa_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Seleciono dados
*----------------------------------------------------------------------*
form f_seleciona_dados .
  select belnr gjahr budat stblg usnam
    from rbkp
    into table it_rbkp
   where bukrs eq p_bukrs
     and budat in s_budat
     and gjahr in r_gjahr
     and j_1bnftype eq ''.

  check sy-subrc eq 0.

  select belnr ebeln bukrs werks
    from rseg
    into table it_rseg
     for all entries in it_rbkp
   where belnr eq it_rbkp-belnr
     and gjahr eq it_rbkp-gjahr
     and buzei eq 1.

  check sy-subrc eq 0.

  select ebeln belnr vgabe matnr
    from ekbe
    into table it_ekbe
     for all entries in it_rseg
   where ebeln eq it_rseg-ebeln
     and belnr eq it_rseg-belnr.

  select ebeln bsart
    from ekko
    into table it_ekko
     for all entries in it_ekbe
   where ebeln eq it_ekbe-ebeln.
endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_DADOS
*&---------------------------------------------------------------------*
*       Monta tabela para relatorio
*----------------------------------------------------------------------*
form f_monta_dados .
  sort: it_rbkp by belnr,
        it_rseg by belnr ebeln,
        it_ekbe by ebeln belnr descending,
        it_ekko by ebeln.
  loop at it_rbkp into wa_rbkp.
    clear wa_relatorio.

    read table it_rseg into wa_rseg with key belnr = wa_rbkp-belnr
                                    binary search.

    read table it_ekbe into wa_ekbe with key ebeln = wa_rseg-ebeln
                                             belnr = wa_rseg-belnr
                                    binary search.
    if ( wa_ekbe-vgabe ne '9' ) and ( wa_ekbe-matnr ne '' ) and
       ( wa_rbkp-stblg eq '' ).
      wa_relatorio-bukrs = wa_rseg-bukrs.
      wa_relatorio-werks = wa_rseg-werks.
      wa_relatorio-belnr = wa_rseg-belnr.
      wa_relatorio-ebeln = wa_rseg-ebeln.
      wa_relatorio-budat = wa_rbkp-budat.
      wa_relatorio-gjahr = wa_rbkp-gjahr.
      wa_relatorio-usnam = wa_rbkp-usnam.
      read table it_ekko into wa_ekko with key ebeln = wa_rseg-ebeln
                                      binary search.
      wa_relatorio-bsart = wa_ekko-bsart.
      append wa_relatorio to it_relatorio.
    endif.
    delete it_ekbe where ebeln eq wa_rseg-ebeln
                     and belnr eq wa_rseg-belnr.
  endloop.
  delete adjacent duplicates from it_relatorio.
  if it_relatorio[] is initial.
    message i000 with 'Não existe informação para esta seleção'.
    stop.
  endif.
endform.                    " F_MONTA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_monta_alv .
  fieldcat ''  c_tabela 'BUKRS'    ''  ''  '' 'Empresa' ''.
  fieldcat ''  c_tabela 'WERKS'    ''  ''  '' 'Filial' ''.
  fieldcat ''  c_tabela 'BELNR'    ''  'X' '' 'Fatura' ''.
  fieldcat ''  c_tabela 'EBELN'    ''  ''  '' 'Pedido' ''.
  fieldcat ''  c_tabela 'BUDAT'    ''  ''  '' 'Data Lançamento' ''.
  fieldcat ''  c_tabela 'BSART'    ''  ''  '' 'Tipo do Pedido' ''.
  fieldcat ''  c_tabela 'USNAM'    ''  ''  '' 'Usuário' ''.
endform.                    " F_MONTA_ALV

*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_executa_alv .
* Variavel Local
  data: vl_repid like sy-repid.

  vl_repid = sy-repid.

  vg_layout-zebra               = 'x'.

* Função para exibir o ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = vl_repid
      i_callback_user_command = 'USER_COMMAND'
      is_layout               = vg_layout
      it_fieldcat             = it_fieldcat[]
      i_default               = 'a'
      i_save                  = 'x'
      it_events               = it_event[]
    tables
      t_outtab                = it_relatorio
    exceptions
      program_error           = 1
      others                  = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " f_executa_alv

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Quando clica no link
*----------------------------------------------------------------------*
form user_command  using ucomm like sy-ucomm
                         selfield type slis_selfield.

  read table it_relatorio into wa_relatorio index selfield-tabindex.
  if selfield-fieldname = 'BELNR'.
    call function 'AUTHORITY_CHECK_TCODE'
      exporting
        tcode  = 'MIR4'
      exceptions
        ok     = 1
        not_ok = 2.
    if sy-subrc = 2.
      message e077(s#) with 'MIR4'.
    endif.
    set parameter id:  'RBN' field wa_relatorio-belnr,
                       'GJR' field wa_relatorio-gjahr,
                       'NCH' field 'X'.
    call transaction 'MIR4' and skip first screen.
  endif.
endform.                    "user_command
