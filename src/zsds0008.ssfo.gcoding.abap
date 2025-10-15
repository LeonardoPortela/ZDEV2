DATA: wl_0051  TYPE zsdt0051,
      wl_0052  TYPE zsdt0052,
      wl_kunnr TYPE kna1-kunnr,
      wl_adrnr TYPE kna1-adrnr,
      wl_t012  TYPE t012,
      wl_tabix TYPE sy-tabix,
      wl_matnr TYPE thead-tdname,
      tl_0053  TYPE TABLE OF zsdt0053 WITH HEADER LINE,
      tl_0055  TYPE TABLE OF zsdt0055 WITH HEADER LINE.

CLEAR: wl_0051, wl_0052, wg_header, wg_cliente,
       wl_kunnr, wl_t012, wl_matnr, wl_tabix.

REFRESH: tl_0053, tl_0055, tg_itens,
         tg_mara, tg_makt, tg_lines, tg_logistica1,
         tg_logistica2, tg_logistica3, tg_logistica.

SELECT SINGLE *
  FROM zsdt0051
  INTO wl_0051
   WHERE nro_sol_ov EQ i_nro_sol_ov
     AND status     EQ 'L'.

IF sy-subrc IS INITIAL.

  wg_header-obs_line1 = 'A entrega do Produto fica condicionada ao pagamento integral do Preço Total pelo(a) Comprador(a).'.

*CONCATENATE wl_0051-dtde_logist+6(2) '/'
*            wl_0051-dtde_logist+4(2) '/'
*            wl_0051-dtde_logist+6(2) into
  wg_cond_pgto-prazo_ret01 = wl_0051-dtde_logist.

*CONCATENATE wl_0051-dtate_logist+6(2) '/'
*            wl_0051-dtate_logist+4(2) '/'
*            wl_0051-dtate_logist+6(2) into

  wg_cond_pgto-prazo_ret02 = wl_0051-dtate_logist.


  SELECT SINGLE bezei
    FROM tvkbt
    INTO wg_header-bezei
     WHERE vkbur EQ wl_0051-vkbur
       AND spras EQ sy-langu.

  SELECT SINGLE adrnr
    FROM kna1
    INTO wl_adrnr
     WHERE kunnr = wl_0051-kunnr.

  IF wl_adrnr IS NOT INITIAL.
    SELECT SINGLE smtp_addr
      FROM adr6
      INTO wg_header-smtp_addr
       WHERE addrnumber = wl_adrnr.
  ENDIF.


  SELECT SINGLE vtext
      FROM tvkot
      INTO wg_header-org_vendas
       WHERE vkorg EQ wl_0051-vkorg
         AND spras EQ sy-langu.

  SELECT SINGLE *
    FROM zsdt0052
    INTO wl_0052
     WHERE nro_sol_ov EQ i_nro_sol_ov.

  IF sy-subrc IS INITIAL.
    SELECT SINGLE text1
      FROM t042z
      INTO wg_cond_pgto-forma_pgto
       WHERE land1 EQ 'BR'
         AND zlsch EQ wl_0052-zlsch.

    SELECT SINGLE vtext
      FROM tvzbt
      INTO wg_cond_pgto-condicao
       WHERE spras EQ sy-langu
         AND zterm EQ wl_0052-zterm.

    SELECT SINGLE ort01
      FROM t001w
      INTO wg_header-cidade
       WHERE werks EQ wl_0051-vkbur.

    SELECT SINGLE text1
      FROM t012t
      INTO wg_cond_pgto-banco
       WHERE spras EQ sy-langu
         AND bukrs EQ wl_0051-vkorg
         AND hbkid EQ wl_0052-hbkid.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE *
        FROM t012
        INTO wl_t012
         WHERE bukrs EQ wl_0051-vkorg
           AND hbkid EQ wl_0052-hbkid
           AND banks EQ 'BR'.

      IF sy-subrc IS INITIAL.
        SELECT SINGLE bnklz
          FROM bnka
          INTO wg_cond_pgto-ag
           WHERE banks EQ 'BR'
             AND bankl EQ wl_t012-bankl.
        CONDENSE wg_cond_pgto-ag NO-GAPS.
        CLEAR: wg_cond_pgto-ag(4).
        CONDENSE wg_cond_pgto-ag NO-GAPS.

      ENDIF.
      SELECT SINGLE bankn bkont
        FROM t012k
        INTO (wg_cond_pgto-conta, wg_cond_pgto-dig)
         WHERE bukrs EQ wl_0051-vkorg
          AND hbkid EQ wl_0052-hbkid.

      CONCATENATE wg_cond_pgto-ag wg_cond_pgto-dig
      INTO wg_cond_pgto-ag SEPARATED BY '-'.
    ENDIF.

    wg_cond_pgto-moeda = wl_0051-waerk.
    wg_cond_pgto-frete = wl_0051-inco1.
    wg_header-logistica = wl_0051-coment_logistica.
    IF wl_0052-valdt IS NOT INITIAL.
      WRITE wl_0052-valdt TO wg_cond_pgto-vencimento.
    ENDIF.
  ENDIF.

  SELECT *
    FROM zsdt0053
    INTO TABLE tl_0053
     WHERE nro_sol_ov EQ i_nro_sol_ov.

  IF sy-subrc IS INITIAL.
    READ TABLE tl_0053 INDEX 1.
    wl_matnr = tl_0053-matnr.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        object          = 'MATERIAL'
        name            = wl_matnr
        id              = 'PRUE'
        language        = sy-langu
      TABLES
        lines           = tg_lines
      EXCEPTIONS
        object          = 1
        id              = 2
        language        = 3
        name            = 4
        not_found       = 5
        reference_check = 6.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tl_0053-werks
      IMPORTING
        output = wl_kunnr.

    SELECT SINGLE stras pstlz stcd1 stcd2 stcd3 ort01 regio
      FROM  kna1
      INTO (wg_header-endereco, wg_header-cep, wg_header-cnpj, wg_header-cpf,
      wg_header-ie, wg_header-municipio, wg_header-uf)
       WHERE kunnr EQ wl_kunnr.

    wg_header-vkaus = wl_0051-vkaus.
    wg_header-nro_sol_ov = wl_0051-nro_sol_ov.
    wg_header-data = wl_0051-data_atual.

    SELECT SINGLE name1 stras pstlz stcd1 stcd2 stcd3 ort01 regio
        FROM  kna1
        INTO (wg_cliente-emissor, wg_cliente-endereco, wg_cliente-cep, wg_cliente-cnpj, wg_cliente-cpf,
        wg_cliente-ie, wg_cliente-municipio, wg_cliente-uf)
         WHERE kunnr EQ wl_0051-kunnr.

    SELECT *
      FROM mara
      INTO TABLE tg_mara
       FOR ALL ENTRIES IN tl_0053
       WHERE matnr EQ tl_0053-matnr.

    SELECT *
      FROM makt
      INTO TABLE tg_makt
       FOR ALL ENTRIES IN tl_0053
       WHERE matnr EQ tl_0053-matnr
         AND spras EQ sy-langu.

    SORT: tg_mara BY matnr,
          tg_makt BY matnr.

    tg_itens[] = tl_0053[].
  ENDIF.

  SELECT *
    FROM zsdt0055
    INTO TABLE tl_0055
     WHERE nro_sol_ov EQ i_nro_sol_ov.

  IF sy-subrc IS INITIAL.
    LOOP AT tl_0055.
      IF sy-tabix LE 1.
        APPEND tl_0055 TO tg_logistica.
      ENDIF.
      ADD 1  TO wl_tabix.
      IF wl_tabix EQ 1.

        APPEND tl_0055 TO tg_logistica1.
      ELSEIF wl_tabix EQ 2.

        APPEND tl_0055 TO tg_logistica2.
      ELSEIF wl_tabix EQ 3.

        APPEND tl_0055 TO tg_logistica3.
        CLEAR: wl_tabix.
      ENDIF.

    ENDLOOP.
  ENDIF.


ENDIF.
