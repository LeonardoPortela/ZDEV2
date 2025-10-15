************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Michely Stefanoski - ABAP                           *
* Data desenv ...: 24.04.2008                                          *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Relatório ALV de código de barra de AP              *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 24.04.2008    Michely              Criação              DEVK903945   *
*                                                                      *
************************************************************************
report  zfir0003.

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*
tables: bsik.

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
      vg_layout          type slis_layout_alv.   "Layout do alv
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: begin of wa_relatorio,
        lifnr            like bsik-lifnr, "Código
        name1            like lfa1-name1, "Fornecedor
        belnr            like bsik-belnr, "Numro Doc.
        zuonr            like bsik-zuonr, "Atribuição
        bldat            like bsik-bldat, "data Doc.
        budat            like bsik-budat, "data Lcto
        zfbdt            like bsik-zfbdt, "data Pgto
        zlsch            like bsik-zlsch, "Forma Pgto
        zlspr            like bsik-zlspr, "Bloqueio
        brcde            like rf05l-brcde,"Código Barra
        hbkid            like bsik-hbkid, "Banco empresa
        dmbtr            like bsik-dmbtr, "Montante MI
        dmbe2            like bsik-dmbe2, "Montante M2
        sgtxt            like bsik-sgtxt, "Texto
        gjahr            like bsik-gjahr, "Exercicio
        bukrs            like bsik-bukrs, "Empresa
        gsber            like bsik-gsber, "Divisão
        buzei            like bsik-buzei, "Item
        color            type slis_t_specialcol_alv, "Cor
      end   of wa_relatorio,

      begin of wa_bsik,
        lifnr            like bsik-lifnr, "Código
        belnr            like bsik-belnr, "Numro Doc.
        buzei            like bsik-buzei,
        bukrs            like bsik-bukrs,
        zuonr            like bsik-zuonr, "Atribuição
        bldat            like bsik-bldat, "data Doc.
        budat            like bsik-budat, "data Lcto
        zfbdt            like bsik-zfbdt, "data Pgto
        zbd1t            like bsik-zbd1t,
        zlsch            like bsik-zlsch, "Forma Pgto
        zlspr            like bsik-zlspr, "Bloqueio
        hbkid            like bsik-hbkid, "Banco empresa
        dmbtr            like bsik-dmbtr, "Montante MI
        dmbe2            like bsik-dmbe2, "Montante M2
        sgtxt            like bsik-sgtxt, "Texto
        gjahr            like bsik-gjahr,
        gsber            like bsik-gsber,
      end   of wa_bsik,

      begin of wa_lfa1,
        lifnr            like lfa1-lifnr,
        name1            like lfa1-name1,
      end   of wa_lfa1,

      begin of wa_bseg,
        bukrs            like bseg-bukrs,
        belnr            like bseg-belnr,
        gjahr            like bseg-gjahr,
        buzei            like bseg-buzei,
        esrnr            like bseg-esrnr,
        esrre            like bseg-esrre,
      end   of wa_bseg.

data: it_relatorio       like standard table of wa_relatorio,
      it_bsik            like standard table of wa_bsik,
      it_lfa1            like standard table of wa_lfa1,
      it_bseg            like standard table of wa_bseg,
      it_cor_normal      type slis_t_specialcol_alv with header line,
      it_cor_altera      type slis_t_specialcol_alv with header line.


*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s00.
parameters: p_bukrs    like bsik-bukrs obligatory.
select-options:
            s_lifnr    for bsik-lifnr obligatory.
parameters: p_data     like bsik-budat obligatory default sy-datum.
selection-screen end   of block b0.

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
initialization.
  set titlebar 'TITULO'.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
* Determinar cores
  perform f_field_color: tables it_cor_normal using 2 0 0,
                         tables it_cor_altera using 5 1 0.
* Seleciona dados na tabela BSIK
  perform f_seleciona_dados.
* Montar dados na estrutura do ALV
  perform f_monta_dados.
* Montar cabeçalho do ALV
  perform f_monta_cabecalho.
* Montar estruduta de ALV
  perform f_monta_estrutura.
* Executa ALV
  perform f_executa_alv.

end-of-selection.


*&---------------------------------------------------------------------*
*&      Form  f_seleciona_dados
*&---------------------------------------------------------------------*
*       Seleciona dados na tabela BSIK
*----------------------------------------------------------------------*
form f_seleciona_dados .
  select lifnr belnr buzei bukrs zuonr bldat budat zfbdt zbd1t zlsch zlspr
         hbkid dmbtr dmbe2 sgtxt gjahr gsber
    from bsik
    into table it_bsik
   where bukrs eq p_bukrs
     and lifnr in s_lifnr
     and budat le p_data
     and zlsch eq 'E'
     and shkzg eq 'H'.

  check sy-subrc eq 0.

  select lifnr name1
    from lfa1
    into table it_lfa1
     for all entries in it_bsik
   where lifnr eq it_bsik-lifnr.

  DATA ETL164C2R95 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L164C2R9157 TYPE FAGL_T_FIELD.
LT_FIELDS_L164C2R9157 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BUZEI' )
 ( LINE = 'ESRNR' )
 ( LINE = 'ESRRE' )
 ).

CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
    EXPORTING IT_FOR_ALL_ENTRIES = IT_BSIK
              I_WHERE_CLAUSE = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
              IT_FIELDLIST = LT_FIELDS_L164C2R9157
    IMPORTING ET_BSEG = ETL164C2R95
    EXCEPTIONS NOT_FOUND = 1.
IF SY-SUBRC = 0 AND LINES( ETL164C2R95 ) > 0.
  MOVE-CORRESPONDING ETL164C2R95 TO IT_BSEG.
  SY-DBCNT = LINES( ETL164C2R95 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.



endform.                    " f_seleciona_dados
*&---------------------------------------------------------------------*
*&      Form  f_monta_dados
*&---------------------------------------------------------------------*
*       Montar dados na estrutura do ALV
*----------------------------------------------------------------------*
form f_monta_dados .
  data: vl_dmbtr(10)     type n,
        vl_vlr           type p decimals 0.
  clear: wa_bsik, wa_bseg, wa_relatorio.
  sort: it_bsik by lifnr bukrs belnr gjahr buzei,
        it_bseg by bukrs belnr gjahr buzei,
        it_lfa1 by lifnr.
  loop at it_bsik into wa_bsik.
    clear wa_bseg.
    wa_relatorio-lifnr = wa_bsik-lifnr.
    wa_relatorio-belnr = wa_bsik-belnr.
    wa_relatorio-zuonr = wa_bsik-zuonr.
    wa_relatorio-bldat = wa_bsik-bldat.
    wa_relatorio-budat = wa_bsik-budat.
    wa_relatorio-zfbdt = wa_bsik-zfbdt + wa_bsik-zbd1t.
    wa_relatorio-zlsch = wa_bsik-zlsch.
    wa_relatorio-zlspr = wa_bsik-zlspr.
    wa_relatorio-hbkid = wa_bsik-hbkid.
    wa_relatorio-dmbtr = wa_bsik-dmbtr.
    wa_relatorio-dmbe2 = wa_bsik-dmbe2.
    wa_relatorio-sgtxt = wa_bsik-sgtxt.
    wa_relatorio-gjahr = wa_bsik-gjahr.
    wa_relatorio-bukrs = wa_bsik-bukrs.
    wa_relatorio-gsber = wa_bsik-gsber.
    wa_relatorio-buzei = wa_bsik-buzei.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_bsik-lifnr
                                             binary search.
    wa_relatorio-name1 = wa_lfa1-name1.

    read table it_bseg into wa_bseg with key bukrs = wa_bsik-bukrs
                                             belnr = wa_bsik-belnr
                                             gjahr = wa_bsik-gjahr
                                             buzei = wa_bsik-buzei
                                             binary search.
*   Preencher o codigo de barra somente se campos diferentes de initial.
    if ( wa_bseg-esrnr is not initial ) and ( wa_bseg-esrre is not initial ).
*   Multiplico por 100 para ignorar o '.'
      vl_vlr = wa_relatorio-dmbtr * 100.
*   Atribuir a variável numerica para adicionar os '0' a esquerda
      vl_dmbtr = vl_vlr.
*   Concatenar para montar o codigo de barra
      concatenate wa_bseg-esrnr
                  wa_bseg-esrre
                  vl_dmbtr
                  into wa_relatorio-brcde.
    else.
      clear wa_relatorio-brcde.
    endif.
    append wa_relatorio to it_relatorio.
  endloop.
endform.                    " f_monta_dados

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
*&      Form  f_monta_estrutura
*&---------------------------------------------------------------------*
*       Monta estrutura de ALV
*----------------------------------------------------------------------*
form f_monta_estrutura .
  perform f_fieldcat using:
            '0' '' 'IT_RELATORIO' 'LIFNR' 'Código'
            10  ''  ''             '' ''  changing it_fieldcat,
            '1' '' 'IT_RELATORIO' 'NAME1' 'Fornecedor'
            30  ''  ''             '' ''  changing it_fieldcat,
            '2' '' 'IT_RELATORIO' 'GSBER' 'Divisão'
            07  ''  ''             '' ''  changing it_fieldcat,
            '3' '' 'IT_RELATORIO' 'BELNR' 'NºDocumento'
            10  ''  ''             '' 'X' changing it_fieldcat,
            '4' '' 'IT_RELATORIO' 'BUZEI' 'Item'
            10  ''  ''             '' ''  changing it_fieldcat,
            '5' '' 'IT_RELATORIO' 'ZUONR' 'Atribuição'
            18  ''  ''             '' ''  changing it_fieldcat,
            '6' '' 'IT_RELATORIO' 'BLDAT' 'Data Doc.'
            10  ''  ''             '' ''  changing it_fieldcat,
            '7' '' 'IT_RELATORIO' 'BUDAT' 'Data Lcto.'
            10  ''  ''             '' ''  changing it_fieldcat,
            '8' '' 'IT_RELATORIO' 'ZFBDT' 'Data Pgto.'
            10  ''  ''             '' ''  changing it_fieldcat,
            '9' '' 'IT_RELATORIO' 'ZLSCH' 'Forma Pgto.'
            11  ''  ''             '' ''  changing it_fieldcat,
           '10' '' 'IT_RELATORIO' 'ZLSPR' 'Bloqueio'
            08  ''  ''             '' ''  changing it_fieldcat,
           '11' '' 'IT_RELATORIO' 'BRCDE' 'Código de barra'
            47  ''  ''             '' ''  changing it_fieldcat,
           '12' '' 'IT_RELATORIO' 'HBKID' 'Bco Empresa'
            11  ''  ''             '' ''  changing it_fieldcat,
           '13' '' 'IT_RELATORIO' 'DMBTR' 'Montante MI'
            15  ''  ''             '' ''  changing it_fieldcat,
           '14' '' 'IT_RELATORIO' 'DMBE2' 'Montante M2'
            15  ''  ''             '' ''  changing it_fieldcat,
           '15' '' 'IT_RELATORIO' 'SGTXT' 'Texto'
            50  ''  ''             '' ''  changing it_fieldcat.
endform.                    " f_monta_estrutura
*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       Executa a ALV com as informações da it_relatório
*----------------------------------------------------------------------*
form f_executa_alv .
* Variavel Local
  data: vl_repid like sy-repid.

  vl_repid = sy-repid.

  it_event-name = slis_ev_top_of_page.
  it_event-form = slis_ev_top_of_page.
  append it_event.

* Determinar a tabela de cores
  vg_layout-coltab_fieldname    = 'COLOR'.
  vg_layout-zebra               = 'X'.

* Função para exibir o ALV
  call function 'REUSE_ALV_GRID_DISPLAY'
  exporting
    i_callback_program       = vl_repid
    i_callback_pf_status_set = 'SET_PF_STATUS'
    i_callback_user_command  = 'USER_COMMAND'
    i_callback_top_of_page   = 'TOP_OF_PAGE'
    is_layout                = vg_layout
*    i_background_id          = c_enjoy
    it_fieldcat              = it_fieldcat[]
    i_default                = 'A'
    i_save                   = 'A'
    it_events                = it_event[]
  tables
    t_outtab                 = it_relatorio
  exceptions
    program_error            = 1
    others                   = 2.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
    with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    "f_executa_alv

*&--------------------------------------------------------------------*
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
form set_pf_status using rt_extab type slis_t_extab.

  set pf-status 'PF_STATUS_ALV'.

endform.                    "set_pf_status

*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&---------------------------------------------------------------------*
*       Ao clicar no nº do documento chamo a transação FB02
*----------------------------------------------------------------------*
form user_command  using p_ucomm like sy-ucomm
                         p_field type slis_selfield.
  data: vl_idx             like p_field-tabindex.
  if p_ucomm eq '&REF'.
* Limpar tabelas internas
    perform f_limpa_tab.
* Seleciona dados na tabela BSIK
    perform f_seleciona_dados.
* Montar dados na estrutura do ALV
    perform f_monta_dados.
    leave to list-processing and return to screen 0.
* Executa ALV
    perform f_executa_alv.
  endif.

  read table it_relatorio into wa_relatorio index p_field-tabindex.
  if p_field-fieldname = 'BELNR'.
    call function 'AUTHORITY_CHECK_TCODE'
      exporting
        tcode  = 'FB02'
      exceptions
        ok     = 1
        not_ok = 2.
    if sy-subrc = 2.
      message e077(s#) with 'FB02'.
    endif.

    set parameter id 'BLN' field wa_relatorio-belnr.
    set parameter id 'BUK' field wa_relatorio-bukrs.
    set parameter id 'GJR' field wa_relatorio-gjahr.
    call transaction 'FB02' and skip first screen.
    vl_idx = p_field-tabindex.
*   Após a alteração efetuada limpo o processo (da transação)
*   e voltoto para a tela inicial para posteriormente executar a ALV
    leave to list-processing and return to screen 0.
    perform f_atualiza_item using wa_relatorio-bukrs
                                  wa_relatorio-belnr
                                  wa_relatorio-gjahr
                                  wa_relatorio-buzei
                                  vl_idx.
  endif.
endform.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  f_monta_cab
*&---------------------------------------------------------------------*
*       Gera o cabeçalho do relatório
*----------------------------------------------------------------------*
form f_monta_cabecalho .
* Colocando os dados para exibição do cabecalho
* Título do relatório
  data: vl_data(10)     type c.

  clear it_header.

  it_header-typ  = 'H'.
  it_header-info = 'Relatório de Código de Barra - AP'.
  append  it_header.

  concatenate p_data+6(2) '.'
              p_data+4(2) '.'
              p_data(4)
         into vl_data.

  it_header-typ  = 'S'.
  it_header-key  = 'Lançadas até'.
  it_header-info = vl_data.
  append  it_header.

endform.                    " f_monta_cab

*&---------------------------------------------------------------------*
*&      Form  top_of_page                                              *
*&---------------------------------------------------------------------*
*      Chama o cabeçalho da ALV                                        *
*----------------------------------------------------------------------*
form top_of_page.
* Cabeçalho
  call function 'REUSE_ALV_COMMENTARY_WRITE'
  exporting
*    i_logo             = c_logo
    it_list_commentary = it_header[].
endform.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  f_limpa_tab
*&---------------------------------------------------------------------*
*       Limpa as tabelas internas
*----------------------------------------------------------------------*
form f_limpa_tab .
  refresh: it_bsik,
           it_lfa1,
           it_bseg,
           it_relatorio.
endform.                    " f_limpa_tab
*&---------------------------------------------------------------------*
*&      Form  f_atualiza_item
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_RELATORIO  text
*----------------------------------------------------------------------*
form f_atualiza_item  using p_bukrs
                            p_belnr
                            p_gjahr
                            p_buzei
                            p_idx.
  data: vl_dmbtr(10)       type n,
        vl_vlr             type p decimals 0.
  wait up to 1 seconds.
  DATA ETL476C2R1367 TYPE TABLE OF BSEG.
DATA LT_FIELDS_L476C2R460 TYPE FAGL_T_FIELD.
LT_FIELDS_L476C2R460 = VALUE #( ( LINE = 'BUKRS' )
 ( LINE = 'BELNR' )
 ( LINE = 'GJAHR' )
 ( LINE = 'BUZEI' )
 ( LINE = 'ESRNR' )
 ( LINE = 'ESRRE' )
 ).
DATA RLDNR_L476C2R4891 TYPE RLDNR.
CALL FUNCTION 'FAGL_GET_LEADING_LEDGER'
  IMPORTING E_RLDNR = RLDNR_L476C2R4891
  EXCEPTIONS NOT_FOUND     = 1
             MORE_THAN_ONE = 2.
IF SY-SUBRC = 0.
CALL FUNCTION 'FAGL_GET_GL_DOCUMENT'
  EXPORTING
    I_RLDNR = RLDNR_L476C2R4891
    I_BUKRS = P_BUKRS
    I_BELNR = P_BELNR
    I_GJAHR = P_GJAHR
    I_BUZEI = P_BUZEI
    IT_FIELDLIST = LT_FIELDS_L476C2R460
  IMPORTING
    ET_BSEG = ETL476C2R1367
  EXCEPTIONS NOT_FOUND = 1.
ENDIF.
IF SY-SUBRC = 0 AND LINES( ETL476C2R1367 ) > 0.
  MOVE-CORRESPONDING ETL476C2R1367 TO IT_BSEG.
  SY-DBCNT = LINES( ETL476C2R1367 ).
ELSE.
  SY-SUBRC = 4.
  SY-DBCNT = 0.
ENDIF.

  wa_relatorio-color[] = it_cor_altera[].
  clear: wa_bseg.
  loop at it_bseg into wa_bseg.
*   Preencher o codigo de barra somente se campos diferentes de initial.
    if ( wa_bseg-esrnr is not initial ) and ( wa_bseg-esrre is not initial ).
*   Multiplico por 100 para ignorar o '.'
      vl_vlr = wa_relatorio-dmbtr * 100.
*   Atribuir a variável numerica para adicionar os '0' a esquerda
      vl_dmbtr = vl_vlr.
*   Concatenar para montar o codigo de barra
      concatenate wa_bseg-esrnr
                  wa_bseg-esrre
                  vl_dmbtr
             into wa_relatorio-brcde.
    else.
      clear wa_relatorio-brcde.
    endif.
  endloop.

  modify it_relatorio index p_idx from wa_relatorio.
  perform f_executa_alv.

endform.                    " f_atualiza_item
*&---------------------------------------------------------------------*
*& Form f_FIELD_COLOR
*&---------------------------------------------------------------------*
* <-- T_COLOR - Tab. com configuração das cores
* --> P_COLOR - Cor
* --> P_INTENSIFIED - Intensificado
* --> P_INVERSE - Inversão de cores
*----------------------------------------------------------------------*
form f_field_color tables t_color type slis_t_specialcol_alv
                   using value(p_color)       type i
                         value(p_intensified) type i
                         value(p_inverse)     type i.
  define f_field_color.
    t_color-color-col = p_color.
    t_color-color-int = p_intensified.
    t_color-color-inv = p_inverse.
    t_color-fieldname = &1.
    append t_color.
  end-of-definition.
* Inicializar tabela
  clear: t_color[], t_color.
* Definir campos
  f_field_color: 'LIFNR', 'NAME1', 'BELNR', 'ZUONR', 'BLDAT', 'BUDAT',
                 'ZFBDT', 'ZLSCH', 'ZLSPR', 'BRCDE', 'HBKID', 'DMBTR',
                 'DMBE2', 'SGTXT', 'GJAHR', 'BUKRS', 'GSBER', 'BUZEI'.
endform. " F_FIELD_COLOR
