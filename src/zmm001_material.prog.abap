************************************************************************
* Responsável ...: Michely Stefanoski - Consultor ABAP                 *
* Data desenv ...: 14.02.2008                                          *
* Tipo de prg ...: executável
* Objetivo    ...: Programa para atualização da descrição dos materiais*
*                  na tabela MARA                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 14.02.2008   Michely               Criação              DEVK903494   *
*                                                                      *
************************************************************************

report zmm001_material
              no standard page heading    "Não exibe cabeçalho standard
              line-size 076               "Comprimento da Linha
              line-count 65.              "Número de Linhas

*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tabelas Transparentes                                                *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Tipos                                                                *
*----------------------------------------------------------------------*
types: begin of ty_arquivo,
         matnr            like makt-matnr, "Nº do material
         maktx            like makt-maktx, "Descrição do material
       end   of ty_arquivo.
*----------------------------------------------------------------------*
* Tabelas Internas Globais                                             *
*----------------------------------------------------------------------*
data: wa_planilha        like alsmex_tabline,  "Work Area p/ Planilha
      wa_arquivo         type ty_arquivo,
      wa_arqerror        like wa_arquivo.

data: it_arquivo         like standard table of wa_arquivo,
      it_arqerror        like standard table of wa_arquivo,
      it_planilha        like standard table of wa_planilha.
*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*
data: vg_begin_col       type i value 1,
      vg_begin_row       type i value 1,
      vg_tabix           like sy-tabix.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
constants: c_end_col      type i value 60,
           c_lines        type i value 64000,
           c_mark         type c value 'X',
           c_n            type c value 'N'.
*----------------------------------------------------------------------*
* Definição de Parâmetros e Opções de Seleção                          *
*----------------------------------------------------------------------*
selection-screen begin of block b0 with frame title text-s00.
parameters: p_arq   like rlgrap-filename obligatory.  "Arq APLIC.SERVER
selection-screen end   of block b0.
*----------------------------------------------------------------------*
* At Selection-Screen                                                  *
*----------------------------------------------------------------------*
at selection-screen on value-request for p_arq.


  call function 'WS_FILENAME_GET'
    exporting
      def_path         = p_arq
      mask             = '*.*'
      mode             = 'O'
      title            = 'Diretório do arquivo de Entrada'
    importing
      filename         = p_arq
    exceptions
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      others           = 5.
*----------------------------------------------------------------------*
* Variáveis Globais                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Field Symbols                                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Definição de Range                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Ponteiro de Objeto                                                   *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Classes Locais                                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Containers                                                           *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
initialization.

*----------------------------------------------------------------------*
* Definição Macros                                                     *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
start-of-selection.
  perform f_carrega_arquivo.
  perform f_atualiza_material.

end-of-selection.

*----------------------------------------------------------------------*
* Top-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* End-of-page                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At User-command                                                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* At Line-selection                                                    *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Início das Sub-Rotinas                                               *
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*&      Form  f_carrega_arquivo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_carrega_arquivo.
  data: vl_flg_del       type c.

  refresh it_planilha.
"Caregar os dados de uma planilha em uma tabela interna
  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = p_arq
      i_begin_col             = vg_begin_col
      i_begin_row             = vg_begin_row
      i_end_col               = c_end_col
      i_end_row               = c_lines
    tables
      intern                  = it_planilha
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    message e000(z01) with text-m01.
  endif.

  sort it_planilha by row col.

  loop at it_planilha into wa_planilha.

    at new row.
      clear: wa_arquivo, vl_flg_del.
    endat.

    if ( wa_planilha-value eq 'DEL' and wa_planilha-col eq 1 ).
      vl_flg_del = c_mark.
    endif.

    check ( vl_flg_del ne c_mark ).

    case wa_planilha-col.
      when  2. wa_arquivo-matnr = wa_planilha-value.
      when  3. wa_arquivo-maktx = wa_planilha-value.
    endcase.

    at end of row.
      append wa_arquivo to it_arquivo.
    endat.

  endloop.

endform.                    " f_carrega_arquivo
*&---------------------------------------------------------------------*
*&      Form  f_atualiza_material
*&---------------------------------------------------------------------*
*       Atualizo a tabela MAKT conforme dados da planilha.
*----------------------------------------------------------------------*
form f_atualiza_material .
  data: vl_rgatu         type n,
        vl_msg           type c length 50.
  clear vl_rgatu.
  loop at it_arquivo into wa_arquivo.
    vl_rgatu = vl_rgatu + 1.
    update makt set maktx = wa_arquivo-maktx
                    maktg = wa_arquivo-maktx
              where matnr = wa_arquivo-matnr.
  endloop.
  concatenate 'Atualizados'
              vl_rgatu
              'registros na tabela MAKT com sucesso'
              into vl_msg separated by space.

  message s000(z01) with vl_msg.
endform.                    " f_atualiza_material
