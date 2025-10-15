*&---------------------------------------------------------------------*
*& Report ZFIR0100_VOLTA_A
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0100_volta_a.

TYPES: BEGIN OF ty_chv,
         obj_key TYPE zfit0100_chv-obj_key,
       END OF ty_chv.

TYPES: BEGIN OF ty_saknr,
         saknr TYPE skb1-saknr,
         bukrs TYPE skb1-bukrs,
       END OF ty_saknr.

DATA: chv_existe  TYPE STANDARD TABLE OF ty_chv INITIAL SIZE 0,
      it_retorn_a TYPE STANDARD TABLE OF ty_saknr INITIAL SIZE 0.
CLEAR: chv_existe, it_retorn_a.

"Verifica se as chaves estão na ZIbContabil ERR e CHV

SELECT DISTINCT a~obj_key
FROM zib_contabil_chv AS a
LEFT JOIN zfit0100_chv AS b ON a~obj_key = b~obj_key
WHERE a~obj_key = b~obj_key
UNION
SELECT DISTINCT a2~obj_key
  FROM zib_contabil_err AS a2
  LEFT JOIN zfit0100_chv AS b2 ON a2~obj_key = b2~obj_key
  WHERE a2~obj_key = b2~obj_key
INTO TABLE @chv_existe.

  IF chv_existe is NOT INITIAL.

SELECT a~hkont as saknr, a~bukrs from zib_contabil as a
  INNER JOIN ZFIT0100_CONT as b on a~hkont = b~racct
  FOR ALL ENTRIES IN @chv_existe
  WHERE a~obj_key = @chv_existe-obj_key
  and a~bukrs not in ( SELECT distinct low AS bukrs FROM tvarvc WHERE name = 'ZFIR0100_LEDGER50' )
  into table @it_retorn_a.

  "Volta o 'A'
  LOOP AT it_retorn_a ASSIGNING FIELD-SYMBOL(<contas_a1>).

        UPDATE skb1 SET mitkz = 'A' WHERE saknr = <contas_a1>-saknr AND bukrs = <contas_a1>-bukrs.
        COMMIT WORK.

    ENDLOOP.

    LOOP AT chv_existe ASSIGNING FIELD-SYMBOL(<remove_chv>).
      DELETE FROM zfit0100_chv WHERE obj_key = <remove_chv>-obj_key.
      COMMIT WORK.
    ENDLOOP.

  ELSE.

    MESSAGE 'Não Existem OBJ_KEY as Serem Verificado!' TYPE 'I' DISPLAY LIKE 'S'.


  ENDIF.
