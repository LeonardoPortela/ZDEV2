FUNCTION ZNFW_BUSCA_SALDO_RETORNO2.
*"--------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_BUKRS) TYPE  ZFIWRT0008-BUKRS
*"     REFERENCE(I_BRANCH) TYPE  ZFIWRT0008-BRANCH
*"     REFERENCE(I_PARVW) TYPE  ZFIWRT0008-PARVW
*"     REFERENCE(I_PARID) TYPE  ZFIWRT0008-PARID
*"     REFERENCE(I_OPERACAO) TYPE  ZFIWRT0008-OPERACAO OPTIONAL
*"     REFERENCE(I_SEQ_LCTO) TYPE  ZFIWRT0008-SEQ_LCTO OPTIONAL
*"     REFERENCE(I_SEQ_MODIFY) TYPE  ZFIWRT0008-SEQ_LCTO OPTIONAL
*"     REFERENCE(I_SHIPFROM) TYPE  LFA1-REGIO OPTIONAL
*"     REFERENCE(I_SHIPTO) TYPE  LFA1-REGIO OPTIONAL
*"     REFERENCE(I_IMOBILIZADO) TYPE  ZFIWRT0008-IMOBILIZADO OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_HEADER) TYPE  ZFIWRT0008
*"  TABLES
*"      ET_ITENS STRUCTURE  ZFIWRT0009
*"  EXCEPTIONS
*"      SEM_SALDO
*"--------------------------------------------------------------------

  TYPES: BEGIN OF ty_0008.
          INCLUDE STRUCTURE zfiwrt0008.
  TYPES: docref_ret TYPE zfiwrt0008-docref,
         END OF ty_0008.
  TYPES: BEGIN OF ty_itens_aux,
          doc_ref TYPE zfiwrt0008-seq_lcto,
          itmnum  TYPE zfiwrt0009-itmnum,
          matnr   TYPE zfiwrt0009-matnr,
          menge   TYPE zfiwrt0009-menge,
         END OF ty_itens_aux.

  RANGES: rg_seq_lcto    FOR zfiwrt0008-seq_lcto,
          rg_imobilizado FOR zfiwrt0008-imobilizado.

  DATA: tl_0008 TYPE TABLE OF ty_0008 WITH HEADER LINE,
        tl_0009 TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_0008_ret TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
        tl_0009_ret TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_itens_aux TYPE TABLE OF ty_itens_aux WITH HEADER LINE,
        wl_0006 TYPE zfiwrt0006,
        wl_index TYPE sy-tabix,
        wl_werks_aux TYPE werks_d,
        wl_parid_aux TYPE J_1BPARID,
        wl_field_aux(20).

  REFRESH: rg_seq_lcto,
           rg_imobilizado.

  IF i_seq_lcto IS NOT INITIAL
  and i_seq_lcto ne '0000000000'.
    rg_seq_lcto-sign    = 'I'.
    rg_seq_lcto-option  = 'EQ'.
    rg_seq_lcto-low     = i_seq_lcto.

    APPEND rg_seq_lcto.
    CLEAR: rg_seq_lcto.
  ENDIF.

  IF i_imobilizado IS NOT INITIAL.
*  and i_seq_lcto ne '0000000000'.
    rg_seq_lcto-sign    = 'I'.
    rg_seq_lcto-option  = 'EQ'.
    rg_seq_lcto-low     = i_imobilizado.

    APPEND rg_seq_lcto.
    CLEAR: rg_seq_lcto.
  ENDIF.

** Busa documentos criados com o mesma empresa, local de negocios e cliente.
  IF i_parvw EQ 'BR'.
    CLEAR: wl_field_aux.
    wl_field_aux = i_parid.
    SHIFT wl_field_aux LEFT DELETING LEADING '0'.
    wl_werks_aux = wl_field_aux.
    SHIFT wl_werks_aux LEFT DELETING LEADING '0'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_werks_aux
      IMPORTING
        output = wl_werks_aux.

    CLEAR: wl_field_aux.
    wl_field_aux = i_branch.
    SHIFT wl_field_aux LEFT DELETING LEADING '0'.
    wl_parid_aux = wl_field_aux.
    SHIFT wl_parid_aux LEFT DELETING LEADING '0'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wl_parid_aux
      IMPORTING
        output = wl_parid_aux.

    SELECT *
        FROM zfiwrt0008
        INTO TABLE tl_0008
         WHERE bukrs    EQ i_bukrs
           AND branch   EQ wl_werks_aux
           AND parvw    EQ i_parvw
           AND parid    EQ wl_parid_aux
           AND loekz    EQ space
           AND seq_lcto IN rg_seq_lcto
           and imobilizado in rg_imobilizado
           AND  ( docref   EQ space
            OR docref   EQ '0000000000' ).
  ELSE.
    SELECT *
      FROM zfiwrt0008
      INTO TABLE tl_0008
       WHERE bukrs    EQ i_bukrs
         AND branch   EQ i_branch
         AND parvw    EQ i_parvw
         AND parid    EQ i_parid
         AND loekz    EQ space
         and imobilizado in rg_imobilizado
         AND seq_lcto IN rg_seq_lcto
         AND  ( docref   EQ space
          OR docref   EQ '0000000000' ).

  ENDIF.
  IF sy-subrc IS INITIAL.
    LOOP AT tl_0008.
      tl_0008-docref_ret = tl_0008-seq_lcto.
      MODIFY tl_0008.
    ENDLOOP.

*** Busca itens dos documentos do cliente.
    SELECT *
      FROM zfiwrt0009
      INTO TABLE tl_0009
       FOR ALL ENTRIES IN tl_0008
       WHERE seq_lcto EQ tl_0008-seq_lcto.

*** busca documentos  criados com referencia aos documentos encontrados do cliente
    SELECT *
      FROM zfiwrt0008
      INTO TABLE tl_0008_ret
      FOR ALL ENTRIES IN tl_0008
       WHERE docref EQ tl_0008-docref_ret
         AND loekz  EQ space.

    IF sy-subrc IS INITIAL.
****  Busa itens dos documentos criados com referencia aos docuemntos dos clientes.
      SELECT *
        FROM zfiwrt0009
        INTO TABLE tl_0009_ret
         FOR ALL ENTRIES IN tl_0008_ret
         WHERE seq_lcto EQ tl_0008_ret-seq_lcto.

    ENDIF.
  ENDIF.

  IF i_seq_modify IS NOT INITIAL.
    DELETE tl_0009_ret WHERE seq_lcto EQ i_seq_modify.

  ENDIF.
  SORT: tl_0008_ret BY seq_lcto.

  LOOP AT tl_0009_ret.
    READ TABLE tl_0008_ret
      WITH KEY seq_lcto = tl_0009_ret-seq_lcto
                  BINARY SEARCH.

    MOVE: tl_0008_ret-docref TO tl_itens_aux-doc_ref,
          tl_0009_ret-itmnum TO tl_itens_aux-itmnum,
          tl_0009_ret-matnr  TO tl_itens_aux-matnr,
          tl_0009_ret-menge  TO tl_itens_aux-menge.

    COLLECT tl_itens_aux.
    CLEAR: tl_0008_ret.
  ENDLOOP.

  IF i_shipto EQ i_shipfrom
  AND i_shipto IS NOT INITIAL
  AND i_shipfrom IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0006
      INTO wl_0006
       WHERE operacao EQ i_operacao
         AND indcoper EQ 'D'.

  ELSEIF i_shipto NE i_shipfrom
  AND i_shipto IS NOT INITIAL
  AND i_shipfrom IS NOT INITIAL.
    SELECT SINGLE *
      FROM zfiwrt0006
      INTO wl_0006
       WHERE operacao EQ i_operacao
         AND indcoper EQ 'F'.
  ENDIF.


  SORT: tl_itens_aux BY doc_ref itmnum.

  LOOP AT tl_0009.
    READ TABLE tl_itens_aux
      WITH KEY doc_ref = tl_0009-seq_lcto
               itmnum  = tl_0009-itmnum
                 BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      SUBTRACT tl_itens_aux-menge FROM tl_0009-menge.
      tl_0009-netwr = tl_0009-menge * tl_0009-netpr.
    ENDIF.

    IF i_parvw EQ 'BR'.
      tl_0009-bwkey = wl_werks_aux.
    ENDIF.
     TL_0009-CFOP = WL_0006-CFOP.
    APPEND tl_0009 TO et_itens.
  ENDLOOP.

  IF i_seq_lcto IS NOT INITIAL.
    READ TABLE tl_0008 INDEX 1.
    MOVE-CORRESPONDING: tl_0008  TO  e_header.
  ENDIF.
  DELETE et_itens WHERE menge EQ 0.

  IF et_itens[] IS INITIAL.
    RAISE sem_saldo.

  ENDIF.

ENDFUNCTION.
