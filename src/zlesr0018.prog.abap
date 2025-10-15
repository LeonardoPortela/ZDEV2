*----------------------------------------------------------------------*
* Report  ZLESR0018                                                    *
* Descrição  : Postos - Autorização de pagamento                       *
* Módulo     :                                  Transação:             *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 01/09/2011*
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  zlesr0018.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon, zmmr.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
     zlest0028.     " Postos - Autorização de pagamento


TYPES: BEGIN OF slis_alv_event,
        name(30),
        form(30),
      END OF slis_alv_event.

TYPES:     slis_t_event TYPE slis_alv_event OCCURS 0.

*----------------------------------------------------------------------*
* TABELA INTERNA
*----------------------------------------------------------------------*
DATA:
 t_bdc              TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
 it_event           TYPE slis_t_event       WITH HEADER LINE             ,   "Eventos
 it_saida           TYPE TABLE OF zlest0028                              .

*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA:
    wa_cont            TYPE REF TO cl_gui_custom_container        , " Objeto Container
    wa_alv             TYPE REF TO cl_gui_alv_grid                , " Objeto ALV
    wa_layout          TYPE lvc_s_layo                            , " Layout da Lista / Fim do DATA
    wa_saida           TYPE zlest0028                             .


*----------------------------------------------------------------------*
* ESTRUTURA ALV  - Tabela Estrutura Colunas do Relatório
*----------------------------------------------------------------------*
DATA:
      it_fieldcat        TYPE slis_t_fieldcat_alv, "Estrutura de saida
      s_variant          TYPE disvariant,
      vg_layout          TYPE slis_layout_alv.   "Layout do alv


*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS:  p_conhec    FOR zlest0028-conhec   NO INTERVALS  NO-EXTENSION,
                 p_ctfret    FOR zlest0028-ctafrete NO INTERVALS  NO-EXTENSION,
                 p_codtrp    FOR zlest0028-codtrp   NO INTERVALS  NO-EXTENSION.

SELECTION-SCREEN: END OF BLOCK b1.

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: form_seleciona,      " Seleção de Dados - Formulário SD
           form_imprimir,       " Impressão
           f_executa_alv.       " Executar o alv para Background e foreground



END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  FORM_SELECIONA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_seleciona.
  SELECT *
  FROM zlest0028
  INTO TABLE it_saida
WHERE conhec   IN p_conhec
  AND ctafrete IN p_ctfret
  AND codtrp   IN p_codtrp.

  IF it_saida IS INITIAL.
    MESSAGE i000(z01) WITH 'Nenhuma Informação Encontrada !'.
  ENDIF.

ENDFORM.                    "form_seleciona

*&---------------------------------------------------------------------*
*&      Form  FORM_ALV
*&---------------------------------------------------------------------*
FORM form_imprimir.
  PERFORM f_fieldcat USING:
      '0' 'X' 'IT_SAIDA' 'CODTRP' 'Código Transportador'
      20  ''  ''             '' '' ''
  CHANGING it_fieldcat,
      '1' 'X' 'IT_SAIDA' 'CHVID'  'Chv Identificação'
      20  ''  ''             '' '' ''
  CHANGING it_fieldcat,
      '2' 'X' 'IT_SAIDA' 'CONHEC' 'Conhecimento'
      20  ''  ''             '' '' ''
  CHANGING it_fieldcat,
      '3' 'X' 'IT_SAIDA' 'CTAFRETE' 'Carta Frete'
      20  ''  ''             '' '' ''
 CHANGING it_fieldcat,
      '4' 'X' 'IT_SAIDA'  'VALOR'  'Valor'
      20  ''  ''             '' '' ''
 CHANGING it_fieldcat,
      '5' 'X' 'IT_SAIDA'  'OBSERVACOES'  'Observações'
      20  ''  ''             '' '' ''
  CHANGING it_fieldcat.

ENDFORM.                    " FORM_ALV
**&---------------------------------------------------------------------
*                                                                      *
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
FORM f_fieldcat USING p_cont p_key  p_tab  p_field p_desc
      p_tam  p_qtde p_fix  p_just p_hot p_sum
CHANGING p_fieldcat TYPE slis_t_fieldcat_alv.

* Tabela interna local
  DATA: tl_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.

  tl_fieldcat-col_pos    = p_cont.
  tl_fieldcat-key        = p_key.
  tl_fieldcat-tabname    = p_tab.
  tl_fieldcat-fieldname  = p_field.
  tl_fieldcat-seltext_l  = p_desc.
  tl_fieldcat-seltext_m  = p_desc.
  tl_fieldcat-seltext_s  = p_desc.
  tl_fieldcat-outputlen  = p_tam.
  tl_fieldcat-quantity   = p_qtde.
  tl_fieldcat-fix_column = p_fix.
  tl_fieldcat-just       = p_just.
  tl_fieldcat-hotspot    = p_hot.
  tl_fieldcat-do_sum     = p_sum.
  APPEND tl_fieldcat TO p_fieldcat.

ENDFORM.                    " f_fieldcatJ1BNFDOC

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    value(p_flag)
                           value(p_fnam)
                           value(p_fval).
  CLEAR t_bdc.

  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.

  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
ENDFORM.                    " Z_LAYOUT


*&---------------------------------------------------------------------*
*&      Form  f_executa_alv
*&---------------------------------------------------------------------*
*       Executar o alv para Background e foreground
*----------------------------------------------------------------------*
FORM f_executa_alv .
* Variavel Local
  DATA: vl_repid LIKE sy-repid.

  vl_repid = sy-repid.

  " it_event-name = slis_ev_top_of_page.
  " it_event-form = slis_ev_top_of_page.
  "APPEND it_event.

*  it_sort-spos = 4.
*  it_sort-fieldname = slis_fieldname-fieldname.
*  it_sort-up = 'X'.
*  it_sort-subtot = 'X'.
*  append it_sort.

  vg_layout-zebra               = 'X'.

* Função para exibir o ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
    i_callback_program       = vl_repid
*    i_callback_pf_status_set = 'SET_PF_STATUS'
*    i_callback_user_command  = 'USER_COMMAND'
    is_layout                = vg_layout
*    i_background_id          = c_enjoy
    it_fieldcat              =  it_fieldcat[]
*   it_sort                  = it_alv_sort[]
    i_default                = 'A'
    i_save                   = 'X'
*    it_events                = it_event[]
  TABLES
    t_outtab                 = it_saida
  EXCEPTIONS
    program_error            = 1
    OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " f_executa_alv
