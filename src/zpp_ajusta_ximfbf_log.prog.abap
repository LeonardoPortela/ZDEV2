*&---------------------------------------------------------------------*
*& Report  ZMMR0030
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 14/04/2015                                              &*
*& Descrição: Interface TRACECOTTON x SAP PP - IN                     &*
*& Transação: PP                                                      &*
*& Request..: DEVK945561                                              &*
*&--------------------------------------------------------------------&*
REPORT  zpp_ajusta_ximfbf_log  MESSAGE-ID ztracecotton.
TYPE-POOLS vrm.

TABLES: zpps_ximfbf_log.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_data FOR sy-datum.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  SELECT *
    FROM zpps_ximfbf_log
    INTO TABLE @DATA(t_log)
   WHERE data IN @s_data
     AND mblnr = @abap_off.

  LOOP AT t_log INTO DATA(w_log).
    w_log-mblnr               = '9999999999'.
    w_log-zrg_atulizado       = 'S'.
    w_log-processado          = 'S'.
    MODIFY zpps_ximfbf_log FROM w_log.
  ENDLOOP.
