FUNCTION zsd_get_data_pw_sati_d100.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_DOCNUM STRUCTURE  ZSD_DOCNUM
*"      T_LIFNR STRUCTURE  ZSDE_LIFNR
*"      T_SAIDA_0001 STRUCTURE  ZSDE_PW_SATI_D100_001
*"      T_SAIDA_0002 STRUCTURE  ZSDE_PW_SATI_D100_001
*"      T_SAIDA_0003 STRUCTURE  ZSDE_PW_SATI_D100_001
*"      T_SAIDA_0004 STRUCTURE  ZSDE_PW_SATI_D100_LIFNR
*"----------------------------------------------------------------------


  DATA: lra_docnum TYPE RANGE OF docnum,
        lra_lifnr  TYPE RANGE OF lifnr.

  DELETE t_docnum WHERE docnum IS INITIAL.  "*-Equalização RISE x PRD - 19.07.2023 - JT

  CHECK t_docnum[] IS NOT INITIAL.          "*-Equalização RISE x PRD - 19.07.2023 - JT

  LOOP AT t_lifnr ASSIGNING FIELD-SYMBOL(<fs_lifnr>).

    APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_lifnr>-lifnr ) TO lra_lifnr.

  ENDLOOP.

  SELECT docnum, tknum
    FROM zcte_identifica
    INTO TABLE @DATA(lt_docnum)
    FOR ALL ENTRIES IN @t_docnum
    WHERE docnum = @t_docnum-docnum.
  IF sy-subrc IS INITIAL.
    SELECT vt~vbeln, vt~parvw, a~taxjurcode
      FROM vtpa AS vt
      INNER JOIN adrc AS a
      ON vt~adrnr = a~addrnumber
     INTO TABLE @DATA(lt_vtpa)
      FOR ALL ENTRIES IN @lt_docnum
     WHERE vbeln = @lt_docnum-tknum
       AND ( vt~parvw = 'PC' OR
             vt~parvw = 'LR' ) .
    IF sy-subrc IS INITIAL.
      SORT lt_vtpa BY vbeln parvw.
    ENDIF.
  ENDIF.

  LOOP AT lt_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum_aux>).

    APPEND INITIAL LINE TO t_saida_0001 ASSIGNING FIELD-SYMBOL(<fs_saida_0001>).

    <fs_saida_0001>-docnum = <fs_docnum_aux>-docnum.

    READ TABLE lt_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa>)
    WITH KEY vbeln = <fs_docnum_aux>-tknum
             parvw = 'PC'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0001>-origem = <fs_vtpa>-taxjurcode.
    ENDIF.

    READ TABLE lt_vtpa ASSIGNING <fs_vtpa>
    WITH KEY vbeln = <fs_docnum_aux>-tknum
             parvw = 'LR'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0001>-destino = <fs_vtpa>-taxjurcode.
    ENDIF.

  ENDLOOP.


  SELECT docnum
     FROM j_1bnfdoc
     INTO TABLE @DATA(lt_docnum2)
     FOR ALL ENTRIES IN @t_docnum
     WHERE docnum = @t_docnum-docnum..
  IF sy-subrc IS INITIAL.
    SELECT docnum, parvw, txjcd
      FROM j_1bnfnad
     INTO TABLE @DATA(lt_j1bnfnad)
      FOR ALL ENTRIES IN @lt_docnum2
     WHERE docnum = @lt_docnum2-docnum
       AND ( parvw = 'T1' OR
             parvw = 'T4' ) .
    IF sy-subrc IS INITIAL.
      SORT lt_j1bnfnad BY docnum parvw.
    ENDIF.
  ENDIF.

  LOOP AT lt_docnum2 ASSIGNING FIELD-SYMBOL(<fs_docnum2>).

    APPEND INITIAL LINE TO t_saida_0002 ASSIGNING FIELD-SYMBOL(<fs_saida_0002>).

    <fs_saida_0002>-docnum = <fs_docnum2>-docnum.

    READ TABLE lt_j1bnfnad ASSIGNING FIELD-SYMBOL(<fs_j1bnfnad>)
    WITH KEY docnum = <fs_docnum2>-docnum
             parvw  = 'T1'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0002>-origem = <fs_j1bnfnad>-txjcd.
    ENDIF.

    READ TABLE lt_j1bnfnad ASSIGNING <fs_j1bnfnad>
    WITH KEY docnum = <fs_docnum2>-docnum
             parvw  = 'T4'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0002>-destino = <fs_j1bnfnad>-txjcd.
    ENDIF.

  ENDLOOP.


  SELECT en_docnum, tknum
      FROM  zlest0034
      INTO TABLE @DATA(lt_zlest0034)
     FOR ALL ENTRIES IN @t_docnum
     WHERE en_docnum = @t_docnum-docnum..
  IF sy-subrc IS INITIAL.

    SELECT vt~vbeln vt~parvw a~taxjurcode
      FROM vtpa AS vt
      INNER JOIN adrc AS a
      ON vt~adrnr = a~addrnumber
     INTO TABLE lt_vtpa
      FOR ALL ENTRIES IN lt_zlest0034
     WHERE vbeln = lt_zlest0034-tknum
       AND ( vt~parvw = 'PC' OR
             vt~parvw = 'LR' ) .
    IF sy-subrc IS INITIAL.
      SORT lt_vtpa BY vbeln parvw.
    ENDIF.
  ENDIF.

  LOOP AT lt_zlest0034 ASSIGNING FIELD-SYMBOL(<fs_zlest0034>).

    APPEND INITIAL LINE TO t_saida_0003 ASSIGNING FIELD-SYMBOL(<fs_saida_0003>).

    <fs_saida_0003>-docnum = <fs_zlest0034>-en_docnum.

    READ TABLE lt_vtpa ASSIGNING <fs_vtpa>
    WITH KEY vbeln = <fs_zlest0034>-tknum
             parvw = 'PC'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0003>-origem = <fs_vtpa>-taxjurcode.
    ENDIF.

    READ TABLE lt_vtpa ASSIGNING <fs_vtpa>
    WITH KEY vbeln = <fs_zlest0034>-tknum
             parvw = 'LR'
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida_0003>-destino = <fs_vtpa>-taxjurcode.
    ENDIF.

  ENDLOOP.

  IF lra_lifnr[] IS NOT INITIAL.
    SELECT lifnr
           txjcd
      FROM lfa1
      INTO TABLE t_saida_0004
     WHERE lifnr IN lra_lifnr.
  ENDIF.

ENDFUNCTION.
