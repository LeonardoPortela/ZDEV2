*&---------------------------------------------------------------------*
*& Include ZFIR0100_SEL
*&---------------------------------------------------------------------*

  TYPES: BEGIN OF ty_selecao,
         rbukrs      TYPE acdoca-rbukrs,
         gjahr       TYPE acdoca-gjahr,
         belnr       TYPE acdoca-belnr,
         obj_key(20) TYPE c,
       END OF ty_selecao.
DATA: it_selecao TYPE STANDARD TABLE OF ty_selecao INITIAL SIZE 0.

FORM SELECT.

  SELECT
  DISTINCT
  a~rbukrs,
  a~gjahr,
  a~belnr
  FROM acdoca AS a
  INNER JOIN zfit0100_cont AS C ON a~racct = C~racct
  WHERE a~rbukrs = @p_emp
  AND a~budat BETWEEN @dtini AND @dtfim
  AND a~gjahr = @p_ano
  AND a~rldnr = '0L'
  AND a~belnr NOT IN ( SELECT DISTINCT b~belnr
  FROM acdoca AS b
  WHERE 1 = 1
  AND b~rbukrs = a~rbukrs
  AND b~budat =  a~budat
  AND b~gjahr = a~gjahr
  AND b~rldnr = '50'
  AND b~racct = a~racct
  AND b~belnr = a~belnr )
  AND substring( a~belnr,1,1 ) IN ('0','1','2','3','4','5','6','7','8','9')
  INTO TABLE @it_selecao.

  LOOP AT it_selecao ASSIGNING FIELD-SYMBOL(<ajusta_selecao>).
    IF <ajusta_selecao>-belnr IS NOT INITIAL.
      PACK <ajusta_selecao>-belnr TO <ajusta_selecao>-belnr.
      CONDENSE <ajusta_selecao>-belnr NO-GAPS.
    ENDIF.

    IF <ajusta_selecao>-obj_key IS INITIAL.
      <ajusta_selecao>-obj_key = 'LED' && <ajusta_selecao>-rbukrs && <ajusta_selecao>-belnr.
    ENDIF.
  ENDLOOP.

  SELECT low AS bukrs FROM tvarvc WHERE name = 'ZFIR0100_LEDGER50' INTO TABLE @DATA(del_bukrs).

  IF del_bukrs IS NOT INITIAL .
    LOOP AT del_bukrs ASSIGNING FIELD-SYMBOL(<del_emp>).
      DELETE it_selecao WHERE rbukrs = <del_emp>.
    ENDLOOP.
  ENDIF.


ENDFORM.
