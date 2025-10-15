*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
* Cliente....: Maggi                                                   *
* Autor......: Daniela Machado                                         *
* Data.......: 14.07.2010                                              *
* Descrição  : Capturar dados do sistema OPUS para carregar a tabela   *
*              ZSDT0001.                                               *
* Projeto....: Maggi - Projeto Evoluir                                 *
* Cód Espec..: GAP_SD03                                                *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*--*
FUNCTION-POOL zsd_rfc_xi.

*----------------------------------------------------------------------*
* Tabelas Internas                                                     *
*----------------------------------------------------------------------*
DATA: BEGIN OF it_log OCCURS 0.
        INCLUDE STRUCTURE zfie_ret_document.
DATA: END OF it_log.

DATA: it_zsdt0001 TYPE TABLE OF zsdt0001.

*----------------------------------------------------------------------*
* Estruturas                                                           *
*----------------------------------------------------------------------*
DATA: wa_log      TYPE zfie_ret_document,
      wa_zsdt0001 TYPE zsdt0001.

*----------------------------------------------------------------------*
* Constantes                                                           *
*----------------------------------------------------------------------*
CONSTANTS: c_14   TYPE zfie_ret_document-interface  VALUE '14',
           c_sd   TYPE zfie_ret_document-id         VALUE 'SD',
           c_897  TYPE zfie_ret_document-num        VALUE '897',
           c_s    TYPE zfie_ret_document-type       VALUE 'S',
           c_e    TYPE zfie_ret_document-type       VALUE 'E'.
