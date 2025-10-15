"Name: \FU:VENDOR_INSERT\SE:BEGIN\EI
ENHANCEMENT 0 ZLES_CHECK_BP.
*
*-169305-28.02.2025-JT-inicio
DATA: l_timestamp     TYPE timestampl,
      l_acao          TYPE char20,
      l_xlfas         TYPE i,
      l_xlfb5         TYPE i,
      l_xlfbk         TYPE i,
      l_xlfza         TYPE i,
      l_xlflr         TYPE i,
      w_zlest0253_tmp TYPE zlest0253_tmp.

l_acao = 'INPUT'.
INCLUDE zlesr0182_tmp.
*-169305-28.02.2025-JT-fim
*
ENDENHANCEMENT.
