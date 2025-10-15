class ZCLFI_UTIL_TAB_ZFIT0208 definition
  public
  final
  create public .

public section.

  methods VERIFICA_CHAVE_PRIMARIA_LOGICA
    importing
      value(IE_ZFIT0208) type ZFIT0208 optional
      value(IV_OPERACAO) type SY-UCOMM
    exporting
      value(EV_MSGERR_CVDUP) type STRING
      !EE_ZFIT0208 type ZFIT0208 .
  methods SEQUENCIA_CHAVE_PRIMARIA
    importing
      value(IV_NUM_RANGE) type NRNR
      value(IV_NAME_NUM) type NROBJ
    exporting
      value(EV_MSGERR_SEQ) type STRING
    returning
      value(RV_NUMBER) type ZFIED_KEYPARAM .
  methods VERIF_COMPOS_RESULT_CNTB_DUPL
    importing
      value(IE_ZFIT0208) type ZFIT0208
      value(IV_OPERACAO) type SY-UCOMM
    exporting
      !EV_MSGERR_RES_CNTB_DUP type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCLFI_UTIL_TAB_ZFIT0208 IMPLEMENTATION.


  METHOD sequencia_chave_primaria.

    DATA: vl_seq TYPE zfied_keyparam.

    FREE rv_number.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = iv_num_range
        object      = iv_name_num
      IMPORTING
        number      = vl_seq.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ev_msgerr_seq.

    ELSE.
      rv_number = |{ vl_seq ALPHA = IN WIDTH = 9 }|.

    ENDIF.

  ENDMETHOD.


  METHOD verifica_chave_primaria_logica.

    SELECT SINGLE * FROM zfit0208
      INTO @DATA(el_t0208)
    WHERE cod_operacao  EQ @ie_zfit0208-cod_operacao
      AND bschl         EQ @ie_zfit0208-bschl
      AND matkl         EQ @ie_zfit0208-matkl
      AND gkont         EQ @ie_zfit0208-gkont
      AND blart         EQ @ie_zfit0208-blart
      AND atribuicao    EQ @ie_zfit0208-atribuicao
      AND anln1         EQ @ie_zfit0208-anln1
      AND sgtxt         EQ @ie_zfit0208-sgtxt
      AND kidno         EQ @ie_zfit0208-kidno
      AND xref1         EQ @ie_zfit0208-xref1
      AND bsart         EQ @ie_zfit0208-bsart
      AND pdd           EQ @ie_zfit0208-pdd
      AND lifnr         EQ @ie_zfit0208-lifnr
      AND saknr         EQ @ie_zfit0208-saknr.

    IF sy-subrc    IS INITIAL  AND
       iv_operacao EQ 'NOVO'.
      ev_msgerr_cvdup = |Registro já cadastrado na tabela de parâmetros. Verificar!|.

    ELSE.
* Verifica Composição de Resultado Contábil Duplicado.
      verif_compos_result_cntb_dupl( EXPORTING ie_zfit0208 = ie_zfit0208
                                               iv_operacao = iv_operacao
                                     IMPORTING ev_msgerr_res_cntb_dup = ev_msgerr_cvdup ).
* Verifica se não deu erro Composição de Resultado Contábil Duplicado.
      IF ev_msgerr_cvdup IS INITIAL.
        ee_zfit0208 = ie_zfit0208.
* Verifica se é criação de um novo registro.
        IF iv_operacao EQ 'NOVO'.
* Criação sequência da chave primária física Tab. ZFIT0208
          ee_zfit0208-keypar = sequencia_chave_primaria( EXPORTING iv_num_range  = '01'
                                                                   iv_name_num   = 'ZFI_T0208'
                                                         IMPORTING ev_msgerr_seq = ev_msgerr_cvdup ).

        ENDIF.

        IF     ev_msgerr_cvdup    IS INITIAL AND
           NOT ee_zfit0208-keypar IS INITIAL.
          CLEAR ev_msgerr_cvdup.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD verif_compos_result_cntb_dupl.

    DATA: vl_saknr      TYPE c LENGTH 500,
          vl_saknr_ori  TYPE c LENGTH 500,
          vl_tail_saknr TYPE c LENGTH 500,
          vl_bsart_ori  TYPE c LENGTH 100,
          vl_tail_bsart TYPE c LENGTH 100,
          vl_lifnr_ori  TYPE c LENGTH 500,
          vl_tail_lifnr TYPE c LENGTH 500.

    CONSTANTS: cl_procen TYPE c VALUE '%'.

    DO.
      IF vl_saknr_ori IS INITIAL.
        vl_saknr_ori = ie_zfit0208-saknr.

      ELSE.
        vl_saknr_ori = vl_tail_saknr.

      ENDIF.

      SPLIT vl_saknr_ori AT ',' INTO vl_saknr vl_tail_saknr.
      vl_saknr = |{ cl_procen }{ vl_saknr }{ cl_procen }|.

      SELECT * FROM zfit0208
        INTO TABLE @DATA(tl_t0208)
      WHERE bschl      EQ   @ie_zfit0208-bschl
        AND matkl      EQ   @ie_zfit0208-matkl
        AND gkont      EQ   @ie_zfit0208-gkont
        AND blart      EQ   @ie_zfit0208-blart
        AND atribuicao EQ   @ie_zfit0208-atribuicao
        AND anln1      EQ   @ie_zfit0208-anln1
        AND sgtxt      EQ   @ie_zfit0208-sgtxt
        AND kidno      EQ   @ie_zfit0208-kidno
        AND xref1      EQ   @ie_zfit0208-xref1
        AND bsart      EQ   @ie_zfit0208-bsart
        AND saknr      LIKE @vl_saknr.

      IF sy-subrc IS INITIAL.
        IF iv_operacao EQ 'CHANGE'.
          DELETE tl_t0208 WHERE keypar EQ ie_zfit0208-keypar.

        ENDIF.

        DO.
          IF vl_bsart_ori IS INITIAL.
            vl_bsart_ori = ie_zfit0208-bsart.

          ELSE.
            vl_bsart_ori = vl_tail_bsart.

          ENDIF.

          SPLIT vl_bsart_ori AT ',' INTO DATA(vl_bsart) vl_tail_bsart.

          DO.

            IF vl_lifnr_ori IS INITIAL.
              vl_lifnr_ori = ie_zfit0208-lifnr.

            ELSE.
              vl_lifnr_ori = vl_tail_lifnr.

            ENDIF.

            SPLIT vl_lifnr_ori AT ',' INTO DATA(vl_lifnr) vl_tail_lifnr.

            LOOP AT tl_t0208 INTO DATA(el_t0208) WHERE bsart CS vl_bsart
                                                   AND lifnr CS vl_lifnr.

              DATA(vl_operacao) = COND #( WHEN iv_operacao EQ 'NOVO' THEN 'novo' ELSE 'atualizado' ).

              ev_msgerr_res_cntb_dup = |O Registro { vl_operacao } já pertence a um cadastrado na tabela. Verificar!|.
              DATA(vl_exit) = abap_on.
              EXIT.

            ENDLOOP.
* Valida saída por chave lógica duplicada.
            IF NOT vl_exit IS INITIAL.
              EXIT.

            ENDIF.

            IF vl_tail_lifnr IS INITIAL.
              EXIT.

            ENDIF.

          ENDDO.
* Valida saída por chave lógica duplicada.
          IF NOT vl_exit IS INITIAL.
            EXIT.

          ENDIF.

          IF vl_tail_bsart IS INITIAL.
            EXIT.

          ENDIF.

        ENDDO.

      ENDIF.
* Valida saída por chave lógica duplicada.
      IF NOT vl_exit IS INITIAL.
        EXIT.

      ENDIF.

      IF vl_tail_saknr IS INITIAL.
        EXIT.

      ENDIF.

    ENDDO.

  ENDMETHOD.
ENDCLASS.
