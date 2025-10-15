*----------------------------------------------------------------------*
***INCLUDE LZ_FIF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_DOUMENTOS_ENERGIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_doumentos_energia TABLES t_docs STRUCTURE zgl_docs_imp
                                t_docs_energia STRUCTURE zgl_docs_energia.

*-CS2021000723 - 18.10.2021 - JT - inicio
  TYPES: BEGIN OF ty_t001,
           bukrs TYPE t001-bukrs,
           butxt TYPE t001-butxt,
           land1 TYPE t001-land1,
         END   OF ty_t001,

         BEGIN OF ty_kna1,
           kunnr TYPE kna1-kunnr,
           name1 TYPE kna1-name1,
         END   OF ty_kna1,

         BEGIN OF ty_docs_aux,
           doc_lcto TYPE zfiwrt0022-seq_lcto.
  TYPES: END   OF ty_docs_aux.

  DATA: tg_docs_aux     TYPE TABLE OF ty_docs_aux,
        wa_docs_aux     TYPE ty_docs_aux,
        wa_docs_energia TYPE zgl_docs_energia,
        wa_kna1         TYPE ty_kna1,
        wa_t001         TYPE ty_t001.

  CHECK t_docs[] IS NOT INITIAL.

  LOOP AT t_docs      INTO DATA(w_docs).
    wa_docs_aux-doc_lcto = w_docs-doc_lcto.
    APPEND wa_docs_aux  TO tg_docs_aux.
  ENDLOOP.

  SELECT *
    FROM zfiwrt0022
    INTO TABLE @DATA(it_zfiwrt0022)
     FOR ALL ENTRIES IN @tg_docs_aux
   WHERE seq_lcto     = @tg_docs_aux-doc_lcto.

  IF it_zfiwrt0022[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt0021
      INTO TABLE @DATA(it_zfiwrt0021)
       FOR ALL ENTRIES IN @it_zfiwrt0022
     WHERE contrato = @it_zfiwrt0022-contrato
       AND bukrs    = @it_zfiwrt0022-bukrs
       AND branch   = @it_zfiwrt0022-branch
       AND kunnr    = @it_zfiwrt0022-kunnr
       AND ano      = @it_zfiwrt0022-ano.
  ENDIF.

  IF it_zfiwrt0021[] IS NOT INITIAL.
    SELECT kunnr, name1
      FROM kna1
      INTO TABLE @DATA(it_kna1)
       FOR ALL ENTRIES IN @it_zfiwrt0021
     WHERE kunnr = @it_zfiwrt0021-kunnr.

    SELECT bukrs, butxt, land1
      FROM t001
      INTO TABLE @DATA(it_t001)
       FOR ALL ENTRIES IN @it_zfiwrt0021
     WHERE bukrs = @it_zfiwrt0021-bukrs.
  ENDIF.

  SORT it_kna1 BY kunnr.
  SORT it_t001 BY bukrs.

  LOOP AT it_zfiwrt0021 INTO DATA(wa_zfiwrt0021).

    CLEAR: wa_kna1, wa_t001.

    READ TABLE it_zfiwrt0022 INTO DATA(wa_zfiwrt0022)
                             WITH KEY contrato = wa_zfiwrt0021-contrato
                                      bukrs    = wa_zfiwrt0021-bukrs
                                      branch   = wa_zfiwrt0021-branch
                                      kunnr    = wa_zfiwrt0021-kunnr
                                      ano      = wa_zfiwrt0021-ano.
    CHECK sy-subrc = 0.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_zfiwrt0021-kunnr
                       BINARY SEARCH.
    READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = wa_zfiwrt0021-bukrs
                       BINARY SEARCH.

    wa_docs_energia-doc_lcto     = wa_zfiwrt0022-seq_lcto.
    wa_docs_energia-contrato     = wa_zfiwrt0021-contrato.

    CASE wa_zfiwrt0021-tipo.
      WHEN 'LP'.
        wa_docs_energia-tipo_contrato = 'Longo Prazo'.
      WHEN 'CP'.
        wa_docs_energia-tipo_contrato = 'Curto Prazo'.
    ENDCASE.

    wa_docs_energia-kunnr        = wa_kna1-kunnr.
    wa_docs_energia-kunnr_desc   = wa_kna1-name1.
    wa_docs_energia-bukrs        = wa_t001-bukrs.
    wa_docs_energia-bukrs_desc   = wa_t001-butxt.
    wa_docs_energia-texto_nota   = wa_zfiwrt0021-texto_nota.
    wa_docs_energia-operacao     = wa_zfiwrt0021-operacao.

    CASE wa_zfiwrt0022-mes.
      WHEN '01'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa01.
        wa_docs_energia-montante = wa_zfiwrt0021-montante01.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '02'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa02.
        wa_docs_energia-montante = wa_zfiwrt0021-montante02.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '03'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa03.
        wa_docs_energia-montante = wa_zfiwrt0021-montante03.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '04'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa04.
        wa_docs_energia-montante = wa_zfiwrt0021-montante04.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '05'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa05.
        wa_docs_energia-montante = wa_zfiwrt0021-montante05.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '06'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa06.
        wa_docs_energia-montante = wa_zfiwrt0021-montante06.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '07'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa07.
        wa_docs_energia-montante = wa_zfiwrt0021-montante07.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '08'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa08.
        wa_docs_energia-montante = wa_zfiwrt0021-montante08.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '09'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa09.
        wa_docs_energia-montante = wa_zfiwrt0021-montante09.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '10'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa10.
        wa_docs_energia-montante = wa_zfiwrt0021-montante10.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '11'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa11.
        wa_docs_energia-montante = wa_zfiwrt0021-montante11.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
      WHEN '12'.
        wa_docs_energia-tarifa   = wa_zfiwrt0021-tarifa12.
        wa_docs_energia-montante = wa_zfiwrt0021-montante12.
        wa_docs_energia-ano      = wa_zfiwrt0021-ano.
    ENDCASE.

    wa_docs_energia-vlr_faturado = wa_docs_energia-tarifa
                                 * wa_docs_energia-montante.

    APPEND wa_docs_energia      TO t_docs_energia.
  ENDLOOP.
*-CS2021000723 - 18.10.2021 - JT - fim

ENDFORM.
