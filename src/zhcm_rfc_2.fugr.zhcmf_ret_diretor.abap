FUNCTION zhcmf_ret_diretor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PERNR) LIKE  PA0001-PERNR
*"  TABLES
*"      T_SAIDA STRUCTURE  ZHCMS_RET_DIRETOR
*"----------------------------------------------------------------------
  DATA: it_superior     TYPE TABLE OF zhcms_ret_sup9002,
        it_superior_aux TYPE TABLE OF zhcms_ret_sup9002,
        lva_achou(1)    TYPE c,
        it_saida        TYPE TABLE OF zhcms_ret_diretor,
        wa_saida        LIKE LINE OF it_saida.



  SELECT SINGLE *
    FROM pa0001
  INTO @DATA(lwa_pa0001)
    WHERE pernr EQ @pernr
     AND  persk IN ('CD', 'IQ')
     AND  endda >= @sy-datum.

  IF sy-subrc = 0.

    wa_saida-pernr_diretor = lwa_pa0001-pernr.

    SELECT SINGLE cname FROM pa0002
      INTO @wa_saida-nome_diretor
      WHERE pernr EQ @pernr
        AND  endda >= @sy-datum.

    SELECT SINGLE cpf_nr FROM pa0465
      INTO @wa_saida-cpf_diretor
      WHERE pernr EQ @pernr
        AND  subty = '0001'
        AND  endda >= @sy-datum.

    APPEND wa_saida TO t_saida.

  ELSE.

    lva_achou = 'N'.

    CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
      EXPORTING
        pernr     = pernr
        tp_gestor = 'I'
      TABLES
        t_saida   = it_superior.

    READ TABLE it_superior INTO DATA(lwa_superior) INDEX 1.

    IF lwa_superior-persk = 'CD' OR lwa_superior-persk  = 'IQ'.

      wa_saida-pernr_diretor = lwa_superior-pernr.
      wa_saida-nome_diretor  = lwa_superior-cname.
      wa_saida-cpf_diretor   = lwa_superior-cpf_nr.

      APPEND wa_saida TO t_saida.

    ELSE.

      WHILE lva_achou = 'N'.

        DATA(lva_pernr) = lwa_superior-pernr.

        CLEAR:  it_superior.
        CALL FUNCTION 'ZHCMF_RET_SUPERIOR_9002'
          EXPORTING
            pernr     = lva_pernr
            tp_gestor = 'I'
          TABLES
            t_saida   = it_superior.

        READ TABLE it_superior INTO lwa_superior INDEX 1.

        IF lwa_superior-persk = 'CD' OR lwa_superior-persk = 'IQ'.
          wa_saida-pernr_diretor = lwa_superior-pernr.
          wa_saida-nome_diretor  = lwa_superior-cname.
          wa_saida-cpf_diretor   = lwa_superior-cpf_nr.
          lva_achou = 'S'.
          APPEND wa_saida TO t_saida.
          EXIT.
        ENDIF.

        IF lva_pernr = lwa_superior-pernr.
* Não achou diretor parametrizado. Chegou no ultimo nivel.
          lva_achou = 'S'.
        ENDIF.

      ENDWHILE.
    ENDIF.

  ENDIF.
ENDFUNCTION.
