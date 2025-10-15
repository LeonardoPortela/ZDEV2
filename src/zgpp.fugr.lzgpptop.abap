function-pool zgpp.                         "MESSAGE-ID ..

* INCLUDE LZGPPD...                          " Local class definition
*&---------------------------------------------------------------------*
*& Form zf_bdc_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_


DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.

DATA: BEGIN OF it_sistemas OCCURS 0,
        sistema    TYPE zftpme_lubrificante-sistema,
        subsistema TYPE zftpme_lubrificante-subsistema,
      END OF it_sistemas.


*----------------------------------------------------------------------*
* Declarações SHDB
*----------------------------------------------------------------------*
DATA: it_bdcdata TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      t_messtab  TYPE TABLE OF          bdcmsgcoll,
      wa_bdcdata LIKE LINE OF           it_bdcdata.


*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
form zf_bdc_data  using    p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  clear wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  append wa_bdcdata to it_bdcdata.

endform.
*&---------------------------------------------------------------------*
*& Form zf_call_transaction
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
form zf_call_transaction  using p_trans
                                p_mode
                                p_upd.
  DATA: BEGIN OF tl_msg OCCURS 0,
          msg TYPE t100-text,
        END OF tl_msg.

*  Data: LV_MSG TYPE BAPIRET2-MESSAGE.

  REFRESH it_msg.

  CALL TRANSACTION p_trans USING it_bdcdata
    MODE p_mode
    MESSAGES INTO it_msg
    UPDATE p_upd.


endform.
