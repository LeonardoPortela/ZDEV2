FUNCTION zhcmf_jano_0001.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DATA) TYPE  BEGDA
*"  TABLES
*"      T_OBJID_NAO STRUCTURE  ZJANO_OBJID_NAO
*"----------------------------------------------------------------------

  DATA: git_hrp1010_nao TYPE STANDARD TABLE OF hrp1010.

  SELECT * FROM hrp1010
    INTO TABLE git_hrp1010_nao
    WHERE plvar = '01'
      AND otype = 'C'
      AND subty = '0007'
      "AND aedtm >= i_data comentado 11/04/2024 - muda na atualização.
      AND hilfm = '002'
      AND endda >= sy-datum
    ORDER BY objid ASCENDING.


  IF git_hrp1010_nao IS NOT INITIAL.
    MOVE-CORRESPONDING git_hrp1010_nao[] TO t_objid_nao[].
  ENDIF.


ENDFUNCTION.
