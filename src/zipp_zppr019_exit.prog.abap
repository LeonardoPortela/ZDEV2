*----------------------------------------------------------------------*
* ID........:                                                          *
* Programa..: ZPP_ZPPR019_EXIT                                         *
* Tipo......: X - EXIT                                                 *
* Transação.: CS02                                                        *
* Descrição.: Cadastro de Listas Técnicas modificadas                  *
* Autor.....: JBARBOSA                                                 *
* Data......: 09.09.2020                                               *
*----------------------------------------------------------------------*
*                     Controle de Alterações                           *
*----------------------------------------------------------------------*
* Data       | Change     | Autor        | Alteração                   *
*----------------------------------------------------------------------*
* 09.09.20   |            |JBARBOSA      | Codificação Inicial         *
*----------------------------------------------------------------------*

* Declaração de ranges
DATA: r_werks TYPE RANGE OF t001w-werks,
      w_werks LIKE LINE OF r_werks.

DATA: t_zppt0025 TYPE TABLE OF zppt0025,
      w_zppt0025 LIKE LINE OF t_zppt0025.

* Declaração de Variáveis
DATA: vl_enviado TYPE sofolenti1-object_id,
      vl_cod_alt TYPE zppt0025-cod_alt,
      vl_uzeit   TYPE sy-uzeit.

DATA(t_delta_stpob) = delta_stpob.
DELETE t_delta_stpob WHERE vbkz = space.

DATA(t_delta) = delta_stpob.
DELETE t_delta WHERE vbkz <> space.

* Verifica-se a lista técnica foi altera
IF t_delta_stpob[] IS NOT INITIAL
      AND sy-tcode = 'CS02'.

* Busca centros\utilização de e-mail cadastros
  SELECT * FROM zppt0024
    INTO TABLE @DATA(t_zppt0024)
    FOR ALL ENTRIES IN @delta_mastb
    WHERE werks = @delta_mastb-werks
      AND stlan = @delta_mastb-stlan.

  REFRESH: r_werks. CLEAR w_werks.
  LOOP AT t_zppt0024 INTO DATA(w_zppt0024).
    w_werks-sign   = 'I'.
    w_werks-option = 'EQ'.
    w_werks-low    = w_zppt0024-werks.
    APPEND w_werks TO r_werks. CLEAR w_werks.
  ENDLOOP.

* Busca Centros
  IF r_werks[] IS NOT INITIAL.

    SELECT * FROM t001w
      INTO TABLE @DATA(t_t001w)
      WHERE werks IN @r_werks.

  ENDIF.

  REFRESH: t_zppt0025. CLEAR: vl_uzeit, vl_cod_alt.

* --------------------------------------------------------
* Dados do Material
* --------------------------------------------------------
  LOOP AT delta_mastb INTO DATA(w_delta_mastb).

* Verifica Centro
    READ TABLE t_t001w INTO DATA(w_t001w) WITH KEY werks = w_delta_mastb-werks.

    IF sy-subrc IS INITIAL.

      LOOP AT t_delta_stpob INTO DATA(w_delta_stpob).

* --------------------------------------------------------
* Verifica status da operação
* --------------------------------------------------------
        READ TABLE t_delta INTO DATA(w_delta)  WITH KEY postp = w_delta_stpob-postp
                                                        posnr = w_delta_stpob-posnr
                                                        idnrk = w_delta_stpob-idnrk.

* --------------------------------------------------------
* Preencher dados do Material
* --------------------------------------------------------
        IF vl_cod_alt IS INITIAL.

          CALL FUNCTION 'NUMBER_GET_NEXT'
            EXPORTING
              nr_range_nr             = '01'
              object                  = 'ZCOD_LIST'
            IMPORTING
              number                  = vl_cod_alt
            EXCEPTIONS
              interval_not_found      = 01
              number_range_not_intern = 02
              object_not_found        = 03
              OTHERS                  = 08.

        ENDIF.

        IF vl_cod_alt IS NOT INITIAL.

          w_zppt0025-cod_alt     = vl_cod_alt.
          w_zppt0025-matnr        = w_delta_mastb-matnr.
          w_zppt0025-werks        = w_delta_mastb-werks.
          w_zppt0025-stlan        = w_delta_mastb-stlan.
          w_zppt0025-idnrk        = w_delta_stpob-idnrk.
          w_zppt0025-posnr        = w_delta_stpob-posnr.

* --------------------------------------------------------
* Preencher dados da alteração
* --------------------------------------------------------
          IF vl_uzeit IS INITIAL.
            vl_uzeit = sy-uzeit.
          ENDIF.

          w_zppt0025-aenam        = sy-uname. "Nome do ùltimo Modificador
          w_zppt0025-aedat        = sy-datum. "Data da Alteração
          w_zppt0025-hr_alteracao = vl_uzeit.
          w_zppt0025-status       = 'P'.      "Status Inicial P-Pendente
          w_zppt0025-tipo         = w_delta_stpob-vbkz."Tipo I-Incluir,U-Alterar,D-Eliminar

          CASE w_zppt0025-tipo .
            WHEN 'I'.
              w_zppt0025-qtd_atual    = w_delta_stpob-menge."Quantidade Atual
              w_zppt0025-qtd_antiga   = w_delta-menge."Quantidade Anterior
            WHEN 'U'.
              w_zppt0025-qtd_atual    = w_delta_stpob-menge."Quantidade Atual
              w_zppt0025-qtd_antiga   = w_delta-menge."Quantidade Anterior
            WHEN 'D'.
              w_zppt0025-qtd_atual    = w_delta-menge."Quantidade Anterior
              w_zppt0025-qtd_antiga   = w_delta_stpob-menge."Quantidade Atual

            WHEN OTHERS.
          ENDCASE.

          APPEND w_zppt0025 TO t_zppt0025.
          CLEAR: w_zppt0025, w_delta_stpob, w_delta.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDLOOP.

  IF t_zppt0025[] IS NOT INITIAL.

* Envia e-mail com os dados da Lista Técnica
    CALL FUNCTION 'ZFPP_EMAIL_LISTA_TECNICA'
      IMPORTING
        e_enviado = vl_enviado
      TABLES
        tl_lista  = t_zppt0025.

    IF vl_enviado IS NOT INITIAL.

      LOOP AT t_zppt0025 ASSIGNING FIELD-SYMBOL(<fs_zppt0025>).
        <fs_zppt0025>-status_email = 'S'.
      ENDLOOP.
    ENDIF.

    MODIFY zppt0025 FROM TABLE t_zppt0025.

  ENDIF.

ENDIF.
