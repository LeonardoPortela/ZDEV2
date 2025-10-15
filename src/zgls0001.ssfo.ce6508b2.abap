
CLEAR: vg_flag_titulo.

if WG_TABELA-TIPO_TAB eq '1'.
  VG_TEXTO_TOTAL = 'Saldo Final Relatório Auxiliar'.
ELSEIF WG_TABELA-TIPO_TAb eq '3'.
  VG_TEXTO_TOTAL = 'Saldo Contábil Final'.
elseif WG_TABELA-TIPO_TAB eq '4'.
  VG_TEXTO_TOTAL = 'Diferença Conciliação'.
elseif WG_TABELA-TIPO_TAB eq '7'.
  VG_TEXTO_TOTAL = 'Total Ações p/ Regularização das Pendências'.
ENDIF.















