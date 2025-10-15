************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 02.05.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Cadastro de estrutura de relatorio DRE              *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 02.05.2008    Michely              Criação              DEVK903994   *
* 31.07.2008    Marcus.Barbara       Alteração            DEVK904576   *
* 31.07.2008    Marcus.Barbara       Alteração            DEVK904601   *
* 19.11.2008    Marcus.Barbara       Alteração            DEVK905224   *
* 28.11.2008    Marcus.Barbara       Alteração            DEVK905250   *
* 13.03.2009    Marcus.Barbara       Alteração            DEVK905650   *
* 02.04.2009    Marcus.Barbara       Alteração            DEVK905741   *
*                                                                      *
************************************************************************
report  zgl002_dre_est no standard page heading  "Não exibe cabeçalho standard
line-size 076               "Comprimento da Linha
line-count 65.              "Número de Linhas

tables: zgl003_dre_est, zgl004_dre_est.

*----------------------------------------------------------------------*
* Macro                                                                *
*----------------------------------------------------------------------*
define exclui_fcode.
  wa_fcode-fcode = &1.
  append wa_fcode to it_fcode.
end-of-definition.

*----------------------------------------------------------------------*
* Declaração Geral
*----------------------------------------------------------------------*
data: begin of mylist occurs 50.       " Internal table hierarchy
        include structure snodetext.
data: end of mylist.


data: txt_report         like dokhl-object.    "Report name for documentation

data: f15                type c, w_campo(30).

data :
      it_objetos             like tadir occurs 0 with header line,
      v_classe               like tadir-devclass,
      cursor(30).

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
type-pools: slis,
            kkblo,
            stree.

*----------------------------------------------------------------------*
* Tabelas Internas (ALV)                                               *
*----------------------------------------------------------------------*
data: it_fieldcat        type slis_t_fieldcat_alv,                   "Estrutura de saida
      it_event           type slis_t_event       with header line,   "Eventos
      it_header          type kkblo_t_listheader with header line,   "Cabeçalho
      vg_layout          type slis_layout_alv,   "Layout do alv
      vg_ucomm           type stree_ucomm.

*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_dre_001.
        include structure zgl001_dre_est.
data: end   of wa_dre_001.

data: begin of wa_dre_002.
        include structure zgl002_dre_est.
data: end   of wa_dre_002.

data: begin of wa_dre_003.
        include structure zgl003_dre_est.
data: end   of wa_dre_003.

data: begin of wa_dre_004.
        include structure zgl004_dre_est.
data: end   of wa_dre_004.

data: begin of wa_conta,
        saknr            like skat-saknr,
        txt50            like skat-txt50,
      end   of wa_conta,

      begin of wa_custo,
        kostl            like cskt-kostl,
        ltext            like cskt-ltext,
      end   of wa_custo,

      begin of wa_ordem,
        aufnr            like aufk-aufnr,
        ktext            like aufk-ktext,
      end   of wa_ordem,

      begin of wa_lucro,
        prctr            like cepct-prctr,
        ltext            like cepct-ltext,
      end   of wa_lucro,

      begin of wa_fcode,
        fcode            like rsmpe-func,
      end   of wa_fcode.


data: it_dre_001 like standard table of wa_dre_001,
      it_dre_002 like standard table of wa_dre_002,
      it_dre_003 like standard table of wa_dre_003,
      it_dre_004     like standard table of wa_dre_004,
      it_dre_004_aux like standard table of wa_dre_004,
      it_conta   like standard table of wa_conta,
      it_custo   like standard table of wa_custo,
      it_ordem   like standard table of wa_ordem,
      it_lucro   like standard table of wa_lucro,
      it_t023t    type table of t023t with header line,
      it_fcode   like standard table of wa_fcode.
*----------------------------------------------------------------------*
* Tela de seleção
*----------------------------------------------------------------------*
*selection-screen begin of block b0 with frame title text-s01.
*parameters:
*            p_bukrs       like zgl001_dre_est-bukrs,
*            p_waers       like zgl001_dre_est-waers.
*selection-screen end   of block b0.

*----------------------------------------------------------------------*
* Telas de Inclusão / Alteração / Exclusão
*----------------------------------------------------------------------*
selection-screen begin of screen 100 as window. "Criar estrutura
selection-screen begin of block b1 with frame title text-s10.
parameters: p_bukrs1     like zgl001_dre_est-bukrs  obligatory,
            p_versn1     like zgl001_dre_est-versn  obligatory,
            p_vstxt1     like zgl001_dre_est-vstxt  obligatory,
            p_waers1     like zgl001_dre_est-waers  obligatory,
            p_funcao     like zgl001_dre_est-funcao obligatory.
selection-screen end   of block b1.
selection-screen end   of screen 100.

selection-screen begin of screen 200 as window. "Alterar estrutura
parameters: p_bukrs2     like zgl001_dre_est-bukrs,
            p_versn2     like zgl001_dre_est-versn.
selection-screen begin of block b2 with frame title text-s20.
parameters: p_vstxt2     like zgl001_dre_est-vstxt  obligatory,
            p_waers2     like zgl001_dre_est-waers  obligatory,
            p_funca2     like zgl001_dre_est-funcao obligatory.
selection-screen end   of block b2.
selection-screen end   of screen 200.

selection-screen begin of screen 300 as window. "Clonar estrutura
selection-screen begin of block b3a with frame title text-s30.
parameters: p_bukrso     like zgl001_dre_est-bukrs obligatory,
            p_versno     like zgl001_dre_est-versn obligatory.
selection-screen end   of block b3a.
selection-screen begin of block b3b with frame title text-s31.
parameters: p_bukrsd     like zgl001_dre_est-bukrs obligatory,
            p_versnd     like zgl001_dre_est-versn obligatory,
            p_funcad     like zgl001_dre_est-funcao obligatory.
selection-screen end   of block b3b.
selection-screen end   of screen 300.

selection-screen begin of screen 400 as window. "Criar Nivel
parameters: p_versn4   like zgl001_dre_est-versn obligatory,
            p_nivel4   type c length 30 obligatory,
            p_lgnvl4   type c length 30 obligatory.
selection-screen begin of block b4 with frame title text-s40.
parameters: p_desnv4   like zgl002_dre_est-desnvl obligatory,
            p_linha4   like zgl002_dre_est-linha.
selection-screen end   of block b4.
selection-screen end   of screen 400.

selection-screen begin of screen 500 as window. "Atribuir Conta
parameters: p_versn5   like zgl001_dre_est-versn obligatory,
            p_nivel5   type c length 30 obligatory.
selection-screen begin of block b5 with frame title text-s50.
select-options:
            s_conta    for zgl003_dre_est-saknr.
selection-screen end   of block b5.
selection-screen end   of screen 500.

selection-screen begin of screen 600 as window. "Objeto de custo
parameters: p_versn6   like zgl001_dre_est-versn obligatory,
            p_nivel6   type c length 30 obligatory,
            p_conta6   like zgl003_dre_est-saknr obligatory.
selection-screen begin of block b6 with frame title text-s60.
select-options:
            s_kostl    for zgl004_dre_est-kostl matchcode object trac_kostl,
            s_aufnr    for zgl004_dre_est-aufnr matchcode object rman_prsp_aufnr,
            s_prctr    for zgl004_dre_est-prctr matchcode object prctr_append.
parameters: s_matkl    like zgl004_dre_est-matkl.
selection-screen end   of block b6.
selection-screen end   of screen 600.

*----------------------------------------------------------------------*
* Event at selection-Screen
*----------------------------------------------------------------------*
at selection-screen.
*  select single * from tstc where tcode eq p_tcode.


*----------------------------------------------------------------------*
* Event at selection-Screen output
*----------------------------------------------------------------------*
* Efetuo o bloquei de determinados campos nas telas de alteração.
at selection-screen output.
  case sy-dynnr.
    when 0200.
      loop at screen.
        if ( screen-name eq 'P_BUKRS2' ) or ( screen-name eq 'P_VERSN2').
          screen-input = 0.
          modify screen.
        endif.
      endloop.
    when 0300.
      loop at screen.
        if ( screen-name eq 'P_BUKRSO' ) or ( screen-name eq 'P_VERSNO').
          screen-input = 0.
          modify screen.
        endif.
      endloop.
    when 0400.
      loop at screen.
        if ( screen-name eq 'P_VERSN4' ) or ( screen-name eq 'P_NIVEL4') or
           ( screen-name eq 'P_LGNVL4' ).
          screen-input = 0.
          modify screen.
        endif.
      endloop.
    when 0500.
      loop at screen.
        if ( screen-name eq 'P_VERSN5' ) or ( screen-name eq 'P_NIVEL5').
          screen-input = 0.
          modify screen.
        endif.
      endloop.
    when 0600.
      loop at screen.
        if ( screen-name eq 'P_VERSN6' ) or ( screen-name eq 'P_NIVEL6') or
        ( screen-name eq 'P_CONTA6' ).
          screen-input = 0.
          modify screen.
        endif.
      endloop.
  endcase.

*----------------------------------------------------------------------*
* Event at selection-screen on                                         *
*----------------------------------------------------------------------*
* No exit no centro de custo busca o centro de lucro e atribui ao campo
*at selection-screen on p_kostl.
*  if p_kostl is not initial.
*    select single prctr
*      from csks
*      into p_prctr
*     where kokrs eq 'MAGI'
*       and kostl eq p_kostl.
*  endif.
*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
initialization.
  set titlebar 'TITULO'.


*----------------------------------------------------------------------*
* Event Start-of-selection
*----------------------------------------------------------------------*
*
start-of-selection.
  perform f_alv_estrutura.

*----------------------------------------------------------------------*
* Event TOP_OF_PAGE.
*----------------------------------------------------------------------*
*
top-of-page.


*----------------------------------------------------------------------*
* Event line-selection
*----------------------------------------------------------------------*
*
at line-selection.


*---------------------------------------------------------------------*
*       FORM build_tree                                               *
*---------------------------------------------------------------------*
form build_tree.

  call function 'RS_TREE_CONSTRUCT'
    tables
      nodetab      = mylist
    exceptions
      tree_failure = 1.

endform.                    "build_tree

*---------------------------------------------------------------------*
*       FORM draw_tree                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form draw_tree.

  call function 'RS_TREE_LIST_DISPLAY'
    exporting
      suppress_node_output  = ' '
      callback_program      = sy-repid
      callback_user_command = 'USER_COMMAND_TREE'  "'NODE_SELECT'
    importing
      f15                   = f15.

endform.                    "draw_tree

*&---------------------------------------------------------------------*
*&      Form  AT_USER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->AT_NODETAB       text
*      -->AT_UCOMM         text
*      -->AT_EXIT          text
*      -->AT_LIST_REFRESH  text
*----------------------------------------------------------------------*
form user_command_tree tables t_nodetab structure seucomm
                        using p_ucomm
                     changing p_exit
                              p_list_refresh.

  select *
    from zgl002_dre_est
    into table it_dre_002
   where bukrs eq wa_dre_001-bukrs
     and versn eq wa_dre_001-versn.

  set pf-status 'TELA'.
  clear wa_fcode.
  case p_ucomm.
    when 'TRSL'.
      case t_nodetab-name.
        when 'NVL'.
          perform f_est002_edit tables t_nodetab.
*          perform f_monta_tabela using wa_dre_001-bukrs
*                                       wa_dre_001-versn
*                                       wa_dre_001-vstxt.
*          perform build_tree.
      endcase.
    when 'CRNVL'.
      if 'EST NVL' cs t_nodetab-name.
        perform f_est002_novo tables t_nodetab.
*        perform f_monta_tabela using wa_dre_001-bukrs
*                                     wa_dre_001-versn
*                                     wa_dre_001-vstxt.
*        perform build_tree.
      else.
        message e000(z01) with 'A este nó não pode atribuir nível.'.
      endif.
    when 'CONTA'.
      if t_nodetab-name eq 'NVL'.
        perform f_est003_novo tables t_nodetab.
*        perform f_monta_tabela using wa_dre_001-bukrs
*                                     wa_dre_001-versn
*                                     wa_dre_001-vstxt.
*        perform build_tree.
      else.
        message e000(z01) with 'A este nó não pode atribuir conta.'.
      endif.
    when 'CUSTO'.
      if t_nodetab-name eq 'CNT'.
        perform f_est004_novo tables t_nodetab.
*        perform f_monta_tabela using wa_dre_001-bukrs
*                                     wa_dre_001-versn
*                                     wa_dre_001-vstxt.
*        perform build_tree.
      else.
        message e000(z01) with 'A este nó não pode atribuir elemento de custo.'.
      endif.
    when 'ELIM'.
      case t_nodetab-name.
        when 'NVL'.
          perform f_est002_excluir tables t_nodetab.
*          perform f_monta_tabela using wa_dre_001-bukrs
*                                       wa_dre_001-versn
*                                       wa_dre_001-vstxt.
*          perform build_tree.
        when 'CNT'.
          perform f_est003_excluir tables t_nodetab.
*          perform f_monta_tabela using wa_dre_001-bukrs
*                                       wa_dre_001-versn
*                                       wa_dre_001-vstxt.
*          perform build_tree.
        when 'OBJ'.
          perform f_est004_excluir tables t_nodetab.
*          perform f_monta_tabela using wa_dre_001-bukrs
*                                       wa_dre_001-versn
*                                       wa_dre_001-vstxt.
*          perform build_tree.
      endcase.
    when 'ATUAL'.
      perform f_monta_tabela using wa_dre_001-bukrs
                                   wa_dre_001-versn
                                   wa_dre_001-vstxt.
      perform build_tree.
  endcase.
  p_list_refresh = 'X'.

endform.                    "AT_USER_COMMAND

*&---------------------------------------------------------------------*
*&      Form  f_alv_estrutura
*&---------------------------------------------------------------------*
*       Em ALV Grid vou exibir o primerio nivel da estrutura de DRE
*----------------------------------------------------------------------*
form f_alv_estrutura .
* Busco dados do primeiro nivel da estrutura de DRE
  perform f_alv_sel_dados.
* Montar estruduta de ALV
  perform f_alv_est.
* Executa ALV
  perform f_alv_executa.
endform.                    " f_alv_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_alv_sel_dados
*&---------------------------------------------------------------------*
*       Seleciono dados na tabela ZGL001_DRE_EST
*----------------------------------------------------------------------*
form f_alv_sel_dados .
  select *
    from zgl001_dre_est
    into table it_dre_001.
endform.                    " f_alv_sel_dados

*&---------------------------------------------------------------------*                                                                      *
*&      Form  f_fieldcat                                               *
*&---------------------------------------------------------------------*
* Preenche a tabela fieldcat                                           *
*----------------------------------------------------------------------*
* p_cont   -> Posição do campo                                         *
* p_key    -> campo chave                                              *
* p_tab    -> tabela interna                                           *
* p_field  -> campo da tabela interna                                  *
* p_desc   -> Descrição do campo                                       *
* p_tam    -> Tamanho do campo de saída                                *
* p_qtde   -> É um campo de to tipo QUAN                               *
* p_fix    -> Congelar a coluna                                        *
* p_just-> -> Alinhamento (R)ight (L)eft (C)ent                        *
*----------------------------------------------------------------------*
form f_fieldcat using p_cont p_key  p_tab  p_field p_desc
                      p_tam  p_qtde p_fix  p_just p_hot
             changing p_fieldcat type slis_t_fieldcat_alv.

* Tabela interna local
  data: tl_fieldcat type slis_t_fieldcat_alv with header line.

  tl_fieldcat-col_pos    = p_cont. "Posição
  tl_fieldcat-key        = p_key.  "
  tl_fieldcat-tabname    = p_tab.  "Tabela interna
  tl_fieldcat-fieldname  = p_field."Campo
  tl_fieldcat-seltext_l  = p_desc. "Descrição longa
  tl_fieldcat-seltext_m  = p_desc. "Descrição media
  tl_fieldcat-seltext_s  = p_desc. "Descrição pequena
  tl_fieldcat-outputlen  = p_tam.  "Tamanho
  tl_fieldcat-quantity   = p_qtde. "Campo quantidade
  tl_fieldcat-fix_column = p_fix.  "Fixar coluna
  tl_fieldcat-just       = p_just. "Alinhar
  tl_fieldcat-hotspot    = p_hot.  "Clique chama evento
  append tl_fieldcat to p_fieldcat.

endform.                    " f_fieldcatJ1BNFDOC
*&---------------------------------------------------------------------*
*&      Form  f_alv_est
*&---------------------------------------------------------------------*
*       Monta estrutura de ALV
*----------------------------------------------------------------------*
form f_alv_est .
  perform f_fieldcat using:
        '0' '' 'IT_DRE_001' 'BUKRS' 'Empresa'
        07  ''  ''             '' ''  changing it_fieldcat,
        '1' '' 'IT_DRE_001' 'VERSN' 'Estrutura'
        09  ''  ''             '' 'X' changing it_fieldcat,
        '2' '' 'IT_DRE_001' 'VSTXT' 'Denominação da Estrutura'
        20  ''  ''             '' ''  changing it_fieldcat,
        '3' '' 'IT_DRE_001' 'WAERS' 'Moeda'
        05  ''  ''             '' ''  changing it_fieldcat,
        '4' '' 'IT_DRE_001' 'FUNCAO' 'Função'
        06  ''  ''             '' ''  changing it_fieldcat.
endform.                    " f_alv_est
*&---------------------------------------------------------------------*
*&      Form  f_alv_executa
*&---------------------------------------------------------------------*
*       Executa a ALV com as informações da it_dre_001
*----------------------------------------------------------------------*
form f_alv_executa .
* Variavel Local
  data: vl_repid like sy-repid.

  vl_repid = sy-repid.

  it_event-name = slis_ev_top_of_page.
  it_event-form = slis_ev_top_of_page.
  append it_event.

* Determinar a tabela de cores
  vg_layout-zebra               = 'X'.

* Função para exibir o ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
  exporting
    i_callback_program       = vl_repid
    i_callback_pf_status_set = 'SET_PF_STATUS'
    i_callback_user_command  = 'USER_COMMAND'
*    i_callback_top_of_page   = 'TOP_OF_PAGE'
    is_layout                = vg_layout
*    i_background_id          = c_enjoy
    it_fieldcat              = it_fieldcat[]
    i_default                = 'A'
    i_save                   = 'A'
*    it_events                = it_event[]
  tables
    t_outtab                 = it_dre_001
  exceptions
    program_error            = 1
    others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.
endform.                    " f_alv_executa

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar na estrutura vai listar todos os niveis
*----------------------------------------------------------------------*
form user_command  using p_ucomm like sy-ucomm
      p_field type slis_selfield.

  read table it_dre_001 into wa_dre_001 index p_field-tabindex.
  case p_ucomm.
    when '&REF'.
      refresh: it_dre_001,
               it_fieldcat.
      leave to list-processing and return to screen 0.
      perform f_alv_estrutura.
    when '&NVO'.
      perform f_est001_novo.
    when '&EXC'.
      perform f_est001_exclui.
    when '&EDT'.
      perform f_est001_altera.
    when '&CLO'.
      perform f_est001_clonar.
  endcase.

  if p_field-fieldname = 'VERSN'.
    set titlebar  '001'.
    set pf-status 'TELA'.
    perform f_monta_tabela using wa_dre_001-bukrs
                                 wa_dre_001-versn
                                 wa_dre_001-vstxt.
    perform build_tree.
    perform draw_tree.
  endif.

endform.                    "user_command

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
form set_pf_status using rt_extab type slis_t_extab.

  set pf-status 'PF_STATUS_ALV'.

endform.                    "set_pf_status

*&---------------------------------------------------------------------*
*&      Form  monta_tabela
*&---------------------------------------------------------------------*
* p_bukrs  -> Empresa                                                  *
* p_versn  -> Versão de estrutura                                      *
*----------------------------------------------------------------------*
form f_monta_tabela using p_bukrs
                        p_versn
                        p_vstxt.

  data: vl_nivel         type c length 30,
        vl_cnt           type n length 6 value 1,
        it_dre_aux2      like standard table of wa_dre_002,
        wa_dre_aux2      like wa_dre_002.

  refresh mylist.

  select *
    from zgl002_dre_est
    into table it_dre_002
   where bukrs eq p_bukrs
     and versn eq p_versn
     and tlevel eq 02.

  mylist-id = p_versn.
  mylist-name = 'EST'.
  mylist-color = 6.
  mylist-intensiv = '1'.
  mylist-text = p_versn.
  mylist-tlength = 10.
  mylist-tlevel = 1.
  mylist-tcolor = 6.
  mylist-tintensiv = '1'.
  mylist-text1 = p_vstxt.
  mylist-tlength1 = 50.
  mylist-tcolor1 = 6.
  mylist-tintensiv1 = '2'.
  append mylist.

  if not it_dre_002[] is initial.
    sort it_dre_002 by ordnv."nivel.
    loop at it_dre_002 into wa_dre_002.
      vl_cnt = vl_cnt + 1.
      clear vl_nivel.
      if wa_dre_002-lgnvl lt 0.
        mylist-id = p_versn.
      else.
        mylist-id = wa_dre_002-lgnvl.
      endif.
      mylist-name = 'NVL'.
      mylist-color = 4.
      mylist-intensiv = '2'.
      perform f_mask_nivel using wa_dre_002-nivel
                        changing vl_nivel.
      mylist-text = vl_nivel.

      call function 'STRING_LENGTH'
        exporting
          string = mylist-text
        importing
          length = mylist-tlength.

      mylist-tlevel = wa_dre_002-tlevel.
      mylist-parent = wa_dre_002-lgnvl.
      mylist-tcolor = 4.
      mylist-tintensiv = '1'.
      mylist-text1 = wa_dre_002-desnvl.
      mylist-tlength1 = 50.
      mylist-tcolor1 = 4.
      mylist-tintensiv1 = '1'.
      append mylist.

      clear vl_nivel.
      concatenate wa_dre_002-ordnv
                  '%' into vl_nivel.
      select *
        from zgl002_dre_est
        into table it_dre_aux2
       where bukrs eq p_bukrs
         and versn eq p_versn
         and ordnv like vl_nivel
         and tlevel ne 02.

      sort it_dre_aux2 by ordnv."nivel.
      loop at it_dre_aux2 into wa_dre_aux2.
        vl_cnt = vl_cnt + 1.
        clear vl_nivel.
        if wa_dre_002-lgnvl lt 0.
          mylist-id = p_versn.
        else.
          mylist-id = wa_dre_002-lgnvl.
        endif.
        mylist-name = 'NVL'.
        mylist-color = 4.
        mylist-intensiv = '2'.
        perform f_mask_nivel using wa_dre_aux2-nivel
                          changing vl_nivel.
        mylist-text = vl_nivel.
        call function 'STRING_LENGTH'
          exporting
            string = mylist-text
          importing
            length = mylist-tlength.
        mylist-tlevel = wa_dre_aux2-tlevel.
        mylist-parent = wa_dre_aux2-lgnvl.
        mylist-tcolor = 4.
        mylist-tintensiv = '1'.
        mylist-text1 = wa_dre_aux2-desnvl.
        mylist-tlength1 = 50.
        mylist-tcolor1 = 4.
        mylist-tintensiv1 = '1'.
        append mylist.

        perform f_monta_003_004 using p_bukrs
                                      p_versn
                                      wa_dre_aux2-nivel
                                      wa_dre_aux2-tlevel.

      endloop.

      perform f_monta_003_004 using p_bukrs
                                    p_versn
                                    wa_dre_002-nivel
                                    wa_dre_002-tlevel.

    endloop.
*    sort mylist by text.
  else.
    format color col_negative intensified on.
    write:/(95) 'Não existe níveis para esta estrutura.'.
  endif.
*  else.
*    format color col_negative intensified on.
*    write:/(95) 'Transação não existe'.
*  endif.

endform.                    " monta_tabela

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_003_004
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_BUKRS  text
*      -->P_P_VERSN  text
*      -->P_WA_DRE_002_NIVEL  text
*----------------------------------------------------------------------*
form f_monta_003_004  using p_bukrs
                            p_versn
                            p_nivel
                            p_tlevel.

  select *
    from zgl003_dre_est
    into table it_dre_003
   where bukrs eq p_bukrs
     and versn eq p_versn
     and nivel eq p_nivel.

  if sy-subrc eq 0.
    sort it_dre_003 by saknr.
    select saknr txt50
      from skat
      into table it_conta
       for all entries in it_dre_003
     where saknr eq it_dre_003-saknr
       and ktopl eq '0050'
       and spras eq 'PT'.
  endif.
  sort it_conta by saknr.
  loop at it_dre_003 into wa_dre_003.
    clear wa_conta.
    read table it_conta into wa_conta with key saknr = wa_dre_003-saknr
                                               binary search.
    mylist-name = 'CNT'.
    mylist-color = 4.
    mylist-intensiv = '2'.
    mylist-text = wa_dre_003-saknr.
    call function 'STRING_LENGTH'
      exporting
        string = mylist-text
      importing
        length = mylist-tlength.
    mylist-tlevel = p_tlevel + 1.
    mylist-tcolor = 12.
    mylist-tintensiv = '1'.
    mylist-text1 = wa_conta-txt50.
    mylist-tlength1 = 50.
    mylist-tcolor1 = 12.
    mylist-tintensiv1 = '1'.
    mylist-text2 = wa_dre_003-nivel.
    append mylist.

    select *
      from zgl004_dre_est
      into table it_dre_004
     where bukrs eq p_bukrs
       and versn eq p_versn
       and nivel eq wa_dre_003-nivel
       and saknr eq wa_dre_003-saknr.

    if sy-subrc eq 0.
      sort it_dre_004 by kostl aufnr prctr.

      clear: it_dre_004_aux[].
      move it_dre_004[] to it_dre_004_aux[].
      sort it_dre_004_aux by kostl.
      delete it_dre_004_aux where kostl eq space.
      delete adjacent duplicates from it_dre_004_aux comparing kostl.

      if not it_dre_004_aux[] is initial.
        select kostl ltext
          from cskt
          into table it_custo
           for all entries in it_dre_004_aux
         where kostl eq it_dre_004_aux-kostl.
      endif.

      clear: it_dre_004_aux[].
      move it_dre_004[] to it_dre_004_aux[].
      sort it_dre_004_aux by aufnr.
      delete it_dre_004_aux where aufnr eq space.
      delete adjacent duplicates from it_dre_004_aux comparing aufnr.

      if not it_dre_004_aux[] is initial.
        select aufnr ktext
          from aufk
          into table it_ordem
           for all entries in it_dre_004_aux
         where aufnr eq it_dre_004_aux-aufnr.
      endif.

      clear: it_dre_004_aux[].
      move it_dre_004[] to it_dre_004_aux[].
      sort it_dre_004_aux by prctr.
      delete it_dre_004_aux where prctr eq space.
      delete adjacent duplicates from it_dre_004_aux comparing prctr.

      if not it_dre_004_aux[] is initial.
        select prctr ltext
          from cepct
          into table it_lucro
           for all entries in it_dre_004_aux
         where prctr eq it_dre_004_aux-prctr.
      endif.

      clear: it_dre_004_aux[].
      move it_dre_004[] to it_dre_004_aux[].
      sort it_dre_004_aux by matkl.
      delete it_dre_004_aux where matkl eq space.
      delete adjacent duplicates from it_dre_004_aux comparing matkl.

      if not it_dre_004_aux[] is initial.
        select * into table it_t023t
          from t023t
           for all entries in it_dre_004_aux
         where spras eq sy-langu
           and matkl eq it_dre_004_aux-matkl.
      endif.

    endif.
    sort: it_custo by kostl,
          it_lucro by prctr,
          it_ordem by aufnr,
          it_t023t  by matkl.
    loop at it_dre_004 into wa_dre_004.
      mylist-name = 'OBJ'.
      mylist-color = 4.
      mylist-intensiv = '2'.
      if wa_dre_004-kostl is not initial.
        mylist-type = 'CC'.
        mylist-text = wa_dre_004-kostl.
        mylist-tlength = 10.
        mylist-tlevel = p_tlevel + 2.
        mylist-tcolor = 8.
        mylist-tintensiv = '1'.
        read table it_custo into wa_custo with key kostl = wa_dre_004-kostl
                                                   binary search.
        mylist-text1 = wa_custo-ltext.
      elseif wa_dre_004-aufnr is not initial.
        mylist-type = 'OI'.
        mylist-text = wa_dre_004-aufnr.
        mylist-tlength = 12.
        mylist-tlevel = p_tlevel + 2.
        mylist-tcolor = 8.
        mylist-tintensiv = '1'.
        read table it_ordem into wa_ordem with key aufnr = wa_dre_004-aufnr
                                                   binary search.
        mylist-text1 = wa_ordem-ktext.
      elseif wa_dre_004-matkl is not initial.
        mylist-type = 'MT'.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            input  = wa_dre_004-matkl
          importing
            output = wa_dre_004-matkl.

        mylist-text = wa_dre_004-matkl.
        mylist-tlength   = 10.
        mylist-tlevel    = p_tlevel + 2.
        mylist-tcolor    = 8.
        mylist-tintensiv = '1'.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            input  = wa_dre_004-matkl
          importing
            output = wa_dre_004-matkl.

        read table it_t023t with key matkl = wa_dre_004-matkl
                                    binary search.
        mylist-text1 = it_t023t-wgbez60.
      else.
        mylist-type = 'CL'.
        mylist-text = wa_dre_004-prctr.
        mylist-tlength = 10.
        mylist-tlevel = p_tlevel + 2.
        mylist-tcolor = 8.
        mylist-tintensiv = '1'.
        read table it_lucro into wa_lucro with key prctr = wa_dre_004-prctr
                                                   binary search.
        mylist-text1 = wa_lucro-ltext.
      endif.
      mylist-tlength1 = 50.
      mylist-tcolor1 = 8.
      mylist-tintensiv1 = '1'.
      mylist-text2 = wa_dre_004-nivel.
      mylist-text3 = wa_dre_004-saknr.
      append mylist.
    endloop.
  endloop.

endform.                    " F_MONTA_003_004


*&---------------------------------------------------------------------*
*&      Form  f_est001_novo
*&---------------------------------------------------------------------*
*       Criar nova estrutura
*----------------------------------------------------------------------*
form f_est001_novo.
  data: vl_bukrs         like zgl001_dre_est-bukrs.
  call selection-screen 100 starting at 10 3.
  if sy-subrc eq 0.
    select single bukrs
      from t001
      into vl_bukrs
     where bukrs eq p_bukrs1.
    if sy-subrc eq 0.
      wa_dre_001-bukrs = p_bukrs1.
      wa_dre_001-versn = p_versn1.
      wa_dre_001-vstxt = p_vstxt1.
      wa_dre_001-waers = p_waers1.
      wa_dre_001-funcao = p_funcao.
      insert into zgl001_dre_est values wa_dre_001.
      message s000(z01) with 'Estrutura' p_versn1 ' cadastrada com sucesso.'.
    else.
      message e000(z01) with 'Empresa' p_bukrs1 ' não encontrada.'.
      exit.
    endif.
  endif.
endform.                    " f_est001_novo
*&---------------------------------------------------------------------*
*&      Form  f_est001_altera
*&---------------------------------------------------------------------*
*       Alterar dados da estrutura
*----------------------------------------------------------------------*
form f_est001_altera .
  p_bukrs2 = wa_dre_001-bukrs.
  p_versn2 = wa_dre_001-versn.
  p_vstxt2 = wa_dre_001-vstxt.
  p_waers2 = wa_dre_001-waers.
  p_funca2 = wa_dre_001-funcao.
  loop at screen.
    if screen-name = 'p_bukrs2'.
      screen-input = 0.
    endif.
  endloop.
  call selection-screen 200 starting at 10 3.
  if sy-subrc eq 0.
    update zgl001_dre_est set vstxt = p_vstxt2 waers = p_waers2
                              funcao = p_funca2
     where bukrs eq p_bukrs2
       and versn eq p_versn2.

    message s000(z01) with 'Estrutura' p_versn2 ' alterada com sucesso.'.
  endif.
endform.                    " f_est001_altera
*&---------------------------------------------------------------------*
*&      Form  f_est001_exclui
*&---------------------------------------------------------------------*
*       Excluir estrutura.
*----------------------------------------------------------------------*
form f_est001_exclui.
  data: vl_res             type c length 1.
  call function 'POPUP_TO_CONFIRM_STEP'
    exporting
      textline1 = 'Deseja excluir a estrutura selecionada?'
      titel     = 'Atenção!'
    importing
      answer    = vl_res.

  if vl_res eq 'J'.
    delete from zgl004_dre_est where bukrs = wa_dre_001-bukrs
                                 and versn = wa_dre_001-versn.

    delete from zgl003_dre_est where bukrs = wa_dre_001-bukrs
                                 and versn = wa_dre_001-versn.

    delete from zgl002_dre_est where bukrs = wa_dre_001-bukrs
                                 and versn = wa_dre_001-versn.

    delete from zgl001_dre_est where bukrs = wa_dre_001-bukrs
                                 and versn = wa_dre_001-versn.

    message s000(z01) with 'Estrutura' wa_dre_001-versn ' excluida com sucesso.'.
  endif.
endform.                    " f_est001_exclui
*&---------------------------------------------------------------------*
*&      Form  f_est001_clonar
*&---------------------------------------------------------------------*
*       Clonar estrutura
*----------------------------------------------------------------------*
form f_est001_clonar .
  data: wa_001             like wa_dre_001,
        wa_002             like wa_dre_002,
        wa_003             like wa_dre_003,
        wa_004             like wa_dre_004.
  p_bukrso = wa_dre_001-bukrs.
  p_versno = wa_dre_001-versn.

  call selection-screen 300 starting at 10 3.
  if sy-subrc eq 0.
    select vstxt waers funcao
      from zgl001_dre_est
      into (wa_001-vstxt, wa_001-waers, wa_001-funcao)
     where bukrs eq p_bukrso
       and versn eq p_versno.

      wa_001-bukrs = p_bukrsd.
      wa_001-versn = p_versnd.

      insert into zgl001_dre_est values wa_001.
    endselect.

    select nivel desnvl linha lgnvl tlevel ordnv
      from zgl002_dre_est
      into (wa_002-nivel, wa_002-desnvl, wa_002-linha,
            wa_002-lgnvl, wa_002-tlevel, wa_002-ordnv)
     where bukrs eq p_bukrso
       and versn eq p_versno.

      wa_002-bukrs = p_bukrsd.
      wa_002-versn = p_versnd.
      insert into zgl002_dre_est values wa_002.
    endselect.

    select nivel saknr
      from zgl003_dre_est
      into (wa_003-nivel, wa_003-saknr)
     where bukrs eq p_bukrso
       and versn eq p_versno.

      wa_003-bukrs = p_bukrsd.
      wa_003-versn = p_versnd.
      insert into zgl003_dre_est values wa_003.
    endselect.

    select nivel saknr kostl aufnr prctr
      from zgl004_dre_est
      into (wa_004-nivel, wa_004-saknr, wa_004-kostl,
            wa_004-aufnr, wa_004-prctr)
     where bukrs eq p_bukrso
       and versn eq p_versno.

      wa_004-bukrs = p_bukrsd.
      wa_004-versn = p_versnd.
      insert into zgl004_dre_est values wa_004.
    endselect.

    message s000(z01) with 'Estrutura' p_versno ' clonada com sucesso para ' p_versnd.
  endif.
endform.                    " f_est001_clonar
*&---------------------------------------------------------------------*
*&      Form  f_est002_novo
*&---------------------------------------------------------------------*
*       Criar novo nível
*----------------------------------------------------------------------*
form f_est002_novo tables t_notabela structure seucomm.
  data: it_dre_nivel     like standard table of wa_dre_002,
        wa_dre_nivel     like wa_dre_002,
        vl_nivel         type c length 30,
        vl_nvlaux        type n length 45,
        vl_aux           type c length 45,
        vl_aux2          type n length 2,
        vl_lgnvl         type c length 30,
        vl_lgt           type i.

  clear: wa_dre_nivel, p_versn4, p_nivel4, p_desnv4, p_linha4,
         p_lgnvl4, vl_aux, vl_nvlaux.

  if t_notabela-name eq 'EST'.
    it_dre_nivel[] = it_dre_002[].
    delete it_dre_nivel where lgnvl gt '0'.
    delete it_dre_nivel where versn ne wa_dre_001-versn.
    sort it_dre_nivel by nivel descending.
    clear wa_dre_002.
    read table it_dre_nivel into wa_dre_002 index 1.
    vl_aux2 = wa_dre_002-nivel + 1.
    wa_dre_nivel-nivel  = vl_aux2.
    wa_dre_nivel-tlevel = 2.
  else.
    vl_aux = t_notabela-text.
    vl_nvlaux = t_notabela-text.

    select *
      from zgl002_dre_est
      into table it_dre_nivel
     where bukrs eq wa_dre_001-bukrs
       and versn eq wa_dre_001-versn
       and lgnvl eq vl_nvlaux.

    sort it_dre_nivel by bukrs versn nivel.
    if it_dre_nivel[] is initial.
      read table it_dre_002 into wa_dre_nivel with key bukrs = wa_dre_001-bukrs
                                                       versn = wa_dre_001-versn
                                                       nivel = vl_nvlaux.
      wa_dre_nivel-lgnvl  = wa_dre_nivel-nivel.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_dre_nivel-nivel
        importing
          output = vl_aux.
      concatenate vl_aux
                 '01' into vl_aux.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          input  = vl_aux
        importing
          output = wa_dre_nivel-nivel.

      wa_dre_nivel-tlevel = wa_dre_nivel-tlevel + 1.
    else.
      sort it_dre_nivel by nivel descending.
      read table it_dre_nivel into wa_dre_002 index 1.
      wa_dre_nivel-lgnvl  = wa_dre_002-lgnvl.
      wa_dre_nivel-nivel  = wa_dre_002-nivel + 1.
      wa_dre_nivel-tlevel = wa_dre_002-tlevel.
    endif.
    perform f_mask_nivel using wa_dre_nivel-lgnvl
                      changing p_lgnvl4.
  endif.

  p_versn4 = wa_dre_001-versn.
  perform f_mask_nivel using wa_dre_nivel-nivel
                    changing p_nivel4.
  if wa_dre_nivel-tlevel le 11.
    call selection-screen 400 starting at 10 3.

    if sy-subrc eq 0.
      translate wa_dre_nivel-nivel using '. '.
      translate wa_dre_nivel-lgnvl using '. '.
      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = wa_dre_nivel-nivel
        importing
          output = wa_dre_nivel-ordnv.
      vl_lgt = strlen( wa_dre_nivel-ordnv ).
      if ( ( vl_lgt eq 1 ) or ( vl_lgt eq 3 )  or
           ( vl_lgt eq 5 ) or ( vl_lgt eq 7 )  or
           ( vl_lgt eq 9 ) or ( vl_lgt eq 11 ) or
           ( vl_lgt eq 13 ) or ( vl_lgt eq 15 ) or
           ( vl_lgt eq 17 ) or ( vl_lgt eq 19 ) or
           ( vl_lgt eq 21 ) or ( vl_lgt eq 23 ) or
           ( vl_lgt eq 25 ) or ( vl_lgt eq 27 ) or
           ( vl_lgt eq 29 ) ).
        concatenate '0'
                    wa_dre_nivel-ordnv
                    into wa_dre_nivel-ordnv.
      endif.
      wa_dre_nivel-versn = wa_dre_001-versn.
      wa_dre_nivel-bukrs = wa_dre_001-bukrs.
      wa_dre_nivel-desnvl = p_desnv4.
      wa_dre_nivel-linha  = p_linha4.
      insert into zgl002_dre_est values wa_dre_nivel.
      message s000(z01) with 'Nível' p_nivel4 ' criado com sucesso'.
    endif.
  else.
    message 'Esta estrutura está limitada a 10 níveis.' type 'E'.
  endif.

endform.                    " f_est002_novo
*&---------------------------------------------------------------------*
*&      Form  f_mask_nivel
*&---------------------------------------------------------------------*
*       Mascara para nivel.
*----------------------------------------------------------------------*
*      -->P_IMPUT_NVL   Nivel sem mascara
*      <--P_OUTPUT_NVL  Nivel com mascara
*----------------------------------------------------------------------*
form f_mask_nivel  using p_input
                changing p_output.
  call function 'ZCONVERTE_NIVEL_MASK'
    exporting
      nvl_imput  = p_input
    importing
      nvl_output = p_output.
endform.                    " f_mask_nivel
*&---------------------------------------------------------------------*
*&      Form  F_EST002_EDIT
*&---------------------------------------------------------------------*
*       Alterar descrição do nivel
*----------------------------------------------------------------------*
form f_est002_edit  tables t_notabela structure seucomm.
  data: vl_aux           type n length 30.
  sort it_dre_002 by bukrs versn nivel.

  vl_aux = t_notabela-text.
  read table it_dre_002 into wa_dre_002 with key bukrs = wa_dre_001-bukrs
                                                 versn = wa_dre_001-versn
                                                 nivel = vl_aux
                                                 binary search.
  perform f_mask_nivel using wa_dre_002-nivel
                    changing p_nivel4.
  p_versn4 = wa_dre_001-versn.
  perform f_mask_nivel using wa_dre_002-lgnvl
                    changing p_lgnvl4.
  p_desnv4 = wa_dre_002-desnvl.
  p_linha4 = wa_dre_002-linha.

  call selection-screen 400 starting at 10 3.

  if sy-subrc eq 0.
    update zgl002_dre_est set desnvl = p_desnv4 linha = p_linha4
     where bukrs eq wa_dre_001-bukrs
       and versn eq p_versn4
       and nivel eq p_nivel4.

    message s000(z01) with 'Nível' p_versn4 ' alterada com sucesso.'.
  endif.

endform.                    " F_EST002_EDIT
*&---------------------------------------------------------------------*
*&      Form  F_EST003_NOVO
*&---------------------------------------------------------------------*
*       Atribuir conta nos níveis
*----------------------------------------------------------------------*
form f_est003_novo  tables t_nodetab structure seucomm.
  data: vl_aux           like zgl004_dre_est-nivel,
        it_dre_conta     like standard table of wa_dre_003,
        wa_dre_conta     like wa_dre_003.

  free s_conta.
  clear: s_conta, vl_aux.
  sort it_dre_002 by bukrs versn nivel.
  wa_dre_conta-bukrs = wa_dre_001-bukrs.
  vl_aux = t_nodetab-text.
  read table it_dre_002 into wa_dre_002 with key bukrs = wa_dre_001-bukrs
                                                 versn = wa_dre_001-versn
                                                 nivel = vl_aux
                                                 binary search.


  perform f_mask_nivel using wa_dre_002-nivel
                    changing p_nivel5.
  p_versn5 = wa_dre_001-versn.
  wa_dre_conta-nivel = p_nivel5.

  call selection-screen 500 starting at 10 3.

  if sy-subrc eq 0.
    loop at s_conta.
      translate wa_dre_conta-nivel using '. '.
      wa_dre_conta-versn = wa_dre_001-versn.
      wa_dre_conta-bukrs = wa_dre_001-bukrs.
      wa_dre_conta-nivel = p_nivel5.
      wa_dre_conta-saknr = s_conta-low.
      insert into zgl003_dre_est values wa_dre_conta.
    endloop.
    message s000(z01) with 'Conta atribuida com sucesso ao nivel ' p_nivel5.
  endif.


endform.                    " F_EST003_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_EST004_NOVO
*&---------------------------------------------------------------------*
*       Atribuir elemento de custo
*----------------------------------------------------------------------*
form f_est004_novo  tables t_nodetab structure seucomm.
  data: vl_cnt           like zgl003_dre_est-saknr,
        vl_nvl           like zgl003_dre_est-nivel,
        vl_prctr         like zgl004_dre_est-prctr,
        it_dre_obj       like standard table of wa_dre_004,
        wa_dre_obj       like wa_dre_004.
  free: s_kostl, s_aufnr, s_prctr, s_matkl.

  vl_nvl = t_nodetab-text2.
  select *
    from zgl003_dre_est
    into table it_dre_003
   where bukrs eq wa_dre_001-bukrs
     and versn eq wa_dre_001-versn
     and nivel eq vl_nvl.

  sort it_dre_003 by bukrs versn nivel saknr.
  wa_dre_obj-bukrs = wa_dre_001-bukrs.
  vl_cnt = t_nodetab-text.

  read table it_dre_003 into wa_dre_003 with key bukrs = wa_dre_001-bukrs
                                                 versn = wa_dre_001-versn
                                                 nivel = vl_nvl
                                                 saknr = vl_cnt
                                                 binary search.

  perform f_mask_nivel using wa_dre_003-nivel
                    changing p_nivel6.

  p_versn6 = wa_dre_001-versn.
  p_conta6 = wa_dre_003-saknr.

  call selection-screen 600 starting at 10 3.

  if sy-subrc eq 0.
    if s_kostl[] is not initial.
      loop at s_kostl.
        wa_dre_obj-bukrs = wa_dre_001-bukrs.
        wa_dre_obj-versn = wa_dre_001-versn.
        wa_dre_obj-nivel = wa_dre_003-nivel.
        wa_dre_obj-saknr = wa_dre_003-saknr.
        wa_dre_obj-kostl = s_kostl-low.
        insert into zgl004_dre_est values wa_dre_obj.
        clear wa_dre_obj.
*        select single prctr
*         from csks
*         into vl_prctr
*        where kokrs eq 'MAGI'
*          and kostl eq s_kostl-low.
*        wa_dre_obj-bukrs = wa_dre_001-bukrs.
*        wa_dre_obj-versn = wa_dre_001-versn.
*        wa_dre_obj-nivel = wa_dre_003-nivel.
*        wa_dre_obj-saknr = wa_dre_003-saknr.
*        wa_dre_obj-prctr = vl_prctr.
*        insert into zgl004_dre_est values wa_dre_obj.
*        clear wa_dre_obj.
      endloop.
    endif.
    if s_aufnr[] is not initial.
      loop at s_aufnr.
        wa_dre_obj-bukrs = wa_dre_001-bukrs.
        wa_dre_obj-versn = wa_dre_001-versn.
        wa_dre_obj-nivel = wa_dre_003-nivel.
        wa_dre_obj-saknr = wa_dre_003-saknr.
        wa_dre_obj-aufnr = s_aufnr-low.
        insert into zgl004_dre_est values wa_dre_obj.
        clear wa_dre_obj.
      endloop.
    endif.
    if s_prctr[] is not initial.
      loop at s_prctr.
        wa_dre_obj-bukrs = wa_dre_001-bukrs.
        wa_dre_obj-versn = wa_dre_001-versn.
        wa_dre_obj-nivel = wa_dre_003-nivel.
        wa_dre_obj-saknr = wa_dre_003-saknr.
        wa_dre_obj-prctr = s_prctr-low.
        insert into zgl004_dre_est values wa_dre_obj.
        clear wa_dre_obj.
      endloop.
    endif.

    if not s_matkl is initial.
      wa_dre_obj-bukrs = wa_dre_001-bukrs.
      wa_dre_obj-versn = wa_dre_001-versn.
      wa_dre_obj-nivel = wa_dre_003-nivel.
      wa_dre_obj-saknr = wa_dre_003-saknr.
      wa_dre_obj-matkl = s_matkl.
      insert into zgl004_dre_est values wa_dre_obj.
      clear wa_dre_obj.
    endif.

    message s000(z01) with 'Objetos atribuidos com sucesso a conta ' p_conta6.
  endif.
endform.                    " F_EST004_NOVO
*&---------------------------------------------------------------------*
*&      Form  F_EST002_EXCLUIR
*&---------------------------------------------------------------------*
*       Eliminar nivel e todos itens relacionados a ele
*----------------------------------------------------------------------*
form f_est002_excluir  tables t_nodetab structure seucomm.
  data: vl_nvl           like zgl004_dre_est-nivel.

  vl_nvl = t_nodetab-text.
  delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                               and versn eq wa_dre_001-versn
                               and nivel eq vl_nvl.
  delete from zgl003_dre_est where bukrs eq wa_dre_001-bukrs
                               and versn eq wa_dre_001-versn
                               and nivel eq vl_nvl.

  delete from zgl002_dre_est where bukrs eq wa_dre_001-bukrs
                               and versn eq wa_dre_001-versn
                               and nivel eq vl_nvl.

endform.                    " F_EST002_EXCLUIR
*&---------------------------------------------------------------------*
*&      Form  F_EST003_EXCLUIR
*&---------------------------------------------------------------------*
*       Excluir todas a conta e os objetos de custo.
*----------------------------------------------------------------------*
form f_est003_excluir  tables   t_nodetab structure seucomm.
  data: vl_cnt           like zgl004_dre_est-saknr,
        vl_nvl           like zgl004_dre_est-nivel.

  vl_nvl = t_nodetab-text2.
  vl_cnt = t_nodetab-text.

  delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                               and versn eq wa_dre_001-versn
                               and nivel eq vl_nvl
                               and saknr eq vl_cnt.

  delete from zgl003_dre_est where bukrs eq wa_dre_001-bukrs
                               and versn eq wa_dre_001-versn
                               and nivel eq vl_nvl
                               and saknr eq vl_cnt.
endform.                    " F_EST003_EXCLUIR
*&---------------------------------------------------------------------*
*&      Form  F_EST004_EXCLUIR
*&---------------------------------------------------------------------*
*       Excluir o objeto de custo
*----------------------------------------------------------------------*
form f_est004_excluir  tables   t_nodetab structure seucomm.
  data: vl_cnt           like zgl004_dre_est-saknr,
        vl_nvl           like zgl004_dre_est-nivel,
        vl_obj           like zgl004_dre_est-kostl,
        vl_obj3          like zgl004_dre_est-matkl,
        vl_obj2          like zgl004_dre_est-aufnr.

  vl_nvl = t_nodetab-text2.
  vl_cnt = t_nodetab-text3.

  case t_nodetab-type.
    when 'CC'.
      vl_obj = t_nodetab-text.
      delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                                   and versn eq wa_dre_001-versn
                                   and nivel eq vl_nvl
                                   and saknr eq vl_cnt
                                   and kostl eq vl_obj.
    when 'OI'.
      vl_obj2 = t_nodetab-text.
      delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                                   and versn eq wa_dre_001-versn
                                   and nivel eq vl_nvl
                                   and saknr eq vl_cnt
                                   and aufnr eq vl_obj2.
    when 'CL'.
      vl_obj = t_nodetab-text.
      delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                                   and versn eq wa_dre_001-versn
                                   and nivel eq vl_nvl
                                   and saknr eq vl_cnt
                                   and prctr eq vl_obj.
    when 'MT'.
      vl_obj3 = t_nodetab-text.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          input  = vl_obj3
        importing
          output = vl_obj3.

      delete from zgl004_dre_est where bukrs eq wa_dre_001-bukrs
                                   and versn eq wa_dre_001-versn
                                   and nivel eq vl_nvl
                                   and saknr eq vl_cnt
                                   and matkl eq vl_obj3.
  endcase.

endform.                    " F_EST004_EXCLUIR
