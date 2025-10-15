*&---------------------------------------------------------------------*
*&  Include           Z_FORM_BUSCA_POR_CENTRO
*&---------------------------------------------------------------------*
*SELECT *
*    INTO CORRESPONDING FIELDS OF TABLE IT_EQKT
*    FROM EQKT AS A
*   INNER JOIN EQUZ AS B ON A~EQUNR = B~EQUNR
*                       AND B~IWERK = TBX_BUSC_CENTRO
*   INNER JOIN ITOB AS C ON A~EQUNR = C~EQUNR
*   WHERE B~DATBI = '99991231'
*     AND C~EQTYP IN ('V','F').

*SELECT *
*   INTO CORRESPONDING FIELDS OF TABLE IT_EQUI
*   FROM EQUI AS A
*  INNER JOIN EQUZ AS B ON A~EQUNR = B~EQUNR
*                      AND B~IWERK = TBX_BUSC_CENTRO
*  INNER JOIN JEST AS C ON A~OBJNR = C~OBJNR
*  WHERE A~EQTYP IN ('V','F')
*    AND B~DATBI  = '99991231'
*    AND C~STAT  <> 'I0076'
*    AND C~STAT  <> 'I0320'
*    AND C~INACT <> 'X'.
*
*IF SY-SUBRC IS NOT INITIAL.
*  MESSAGE W836(SD) WITH 'Nenhum equipamento encontrado.'.
*ENDIF.
