FUNCTION zhcmf_uniorg_estrutura.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(UNIORG) TYPE  HROBJID
*"  EXPORTING
*"     REFERENCE(T_SAIDA) TYPE  ZHCME0003_T
*"----------------------------------------------------------------------

  DATA: r_uniorg TYPE RANGE OF hrp1001-objid.

  CHECK uniorg IS NOT INITIAL.

  SELECT *
    FROM hrp1001
    INTO TABLE @DATA(it_hrp1001)
  WHERE otype EQ 'O'
    AND objid EQ @uniorg
    AND plvar EQ '01'
    AND rsign EQ 'B'
    AND relat EQ '002'
    AND endda >= @sy-datum.

  CHECK sy-subrc IS INITIAL.

  LOOP AT it_hrp1001 INTO DATA(wa_hrp1001).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_hrp1001-objid ) TO r_uniorg.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_hrp1001-sobid ) TO r_uniorg.
  ENDLOOP.

  SORT r_uniorg.
  DELETE ADJACENT DUPLICATES FROM r_uniorg.

  SELECT *
    FROM hrp1000
    INTO TABLE @DATA(it_hrp1000)
    WHERE plvar EQ '01'
      AND otype EQ 'O'
      AND objid IN @r_uniorg
      AND langu EQ @sy-langu
      AND endda >= @sy-datum.

  LOOP AT it_hrp1001 INTO wa_hrp1001.
    READ TABLE it_hrp1000 INTO DATA(wa_hrp1000_pai) WITH KEY objid = wa_hrp1001-objid.
    READ TABLE it_hrp1000 INTO DATA(wa_hrp1000_filho) WITH KEY objid = wa_hrp1001-sobid.

    APPEND VALUE #(
                    cod_uniorg_pai    = wa_hrp1000_pai-objid
                    nome_uniorg_pai   = wa_hrp1000_pai-stext
                    cod_uniorg_filho  = wa_hrp1000_filho-objid
                    nome_uniorg_filho = wa_hrp1000_filho-stext
                    prioridade        = wa_hrp1001-priox
     ) TO t_saida.
  ENDLOOP.

ENDFUNCTION.
