"Name: \FU:J_1BNF_FILL_ADDITIONAL_FIELDS\SE:END\EI
ENHANCEMENT 0 ZJ_1BNF_FILL_MODBC_RATE.
*
  DATA: vl_item TYPE bapiwrbtr.
  DATA: vl_pis_cofins TYPE bapiwrbtr.
  DATA:  w_setlinet TYPE setlinet.

  DATA: BEGIN OF t_cfop OCCURS 0,
          nf TYPE j_1bnfdoc-nftype,
        END OF t_cfop.

  SELECT * FROM setlinet
      INTO w_setlinet
      WHERE setname = 'MIRO_CFOP'.
    t_cfop-nf = w_setlinet-descript(2).
    APPEND t_cfop TO t_cfop.
  ENDSELECT.


  LOOP AT ct_nflin ASSIGNING FIELD-SYMBOL(<fs_ct_nflin>)
    WHERE mwskz IS NOT INITIAL.

    SELECT SINGLE * INTO @DATA(wa_zmmt0155)
      FROM zmmt0125
     WHERE mwskz EQ @<fs_ct_nflin>-mwskz.

    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IMPORT p1 = vl_item FROM MEMORY ID 'MZMMR019VLRITEM'.
    cs_nfdoc-nftot = vl_item.
    <fs_ct_nflin>-nfnet = vl_item.
    <fs_ct_nflin>-nfnett = vl_item.

    vl_pis_cofins = 0.

    LOOP AT ct_nfstx ASSIGNING FIELD-SYMBOL(<nfstx>)
      WHERE docnum EQ <fs_ct_nflin>-docnum
        AND itmnum EQ <fs_ct_nflin>-itmnum
        AND taxval IS NOT INITIAL.

      ADD <nfstx>-taxval TO vl_pis_cofins.

      CASE <nfstx>-taxgrp.
        WHEN 'COFI'.
          <nfstx>-rate = wa_zmmt0155-nm_ali_cofins.
        WHEN 'PIS'.
          <nfstx>-rate = wa_zmmt0155-nm_ali_pis.
      ENDCASE.

    ENDLOOP.

    <fs_ct_nflin>-netwr = vl_item - vl_pis_cofins.
    <fs_ct_nflin>-netwrt = <fs_ct_nflin>-netwr.

  ENDLOOP.


  TYPES: ty_lineitem TYPE TABLE OF j_1bnflin.
*Incio de alteração - Fmartins - 12/08/2022 - CS0988372
  FIELD-SYMBOLS: <fs_cfop> TYPE j_1bcfop.
  DATA: w_cfop(1) VALUE 'N'.

*---> CS1071560 / IR129893 --->
  READ TABLE t_cfop WITH KEY nf = ls_nfdoc-nftype.
  IF sy-subrc EQ 0.
    w_cfop = 'S'.
  ENDIF.
*<--- CS1071560 / IR129893 <---

  ASSIGN ('(SAPLJ1BI)X_LINEITEM-CFOP') TO <fs_cfop>.

  IF <fs_cfop> IS ASSIGNED.
    IF <fs_cfop> IS INITIAL AND w_cfop = 'S'.
      CASE sy-ucomm.
        WHEN 'PB'.
          MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'
          RAISING no_info_found.
        WHEN 'BU'.
          MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'.
          EXIT.
        WHEN 'EXECUTAR'. "BUG169461
          MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'.
          EXIT.
        WHEN OTHERS.
          MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'W'.
          EXIT.
      ENDCASE.
    ENDIF.
  ELSEIF w_cfop = 'S' AND sy-cprog NE 'ZWRJ0001' AND sy-cprog NE 'ZGL034' and "  INSERIDO, esta dando erro nsa ZNFW002 por ex.. ALRS
                          sy-cprog NE 'ZLESR0181_JOB'.  "CS202200102-25.04.2024-JT-#96439-inicio
    IF sy-ucomm EQ 'PB'.
      MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'
      RAISING no_info_found.
    ELSEIF sy-ucomm EQ 'BU'.
      MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'.
    ELSE.
      MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'W'.
    ENDIF.
    EXIT.
  ENDIF.

*Fim de alteração - Fmartins - 12/08/2022 - CS0988372
  FIELD-SYMBOLS: <lineitem> TYPE ty_lineitem.
  ASSIGN ('(SAPLJ1BI)X_LINEITEM[]') TO <lineitem>.


*  IF LS_NFDOC-NFTYPE = 'NS'.
  IF w_cfop = 'S'.
    IF <lineitem> IS ASSIGNED.
      LOOP AT <lineitem> INTO DATA(lw_lineitem).
        IF lw_lineitem-cfop IS INITIAL.
          CASE sy-ucomm.
              WHEN 'BU'.
              MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'E'.
              EXIT.
              WHEN OTHERS.
              MESSAGE 'Obrigatorio preenchimento do campo CFOP' TYPE 'W'.
              EXIT.
          ENDCASE.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.


ENDENHANCEMENT.
