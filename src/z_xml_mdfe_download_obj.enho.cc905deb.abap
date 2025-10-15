"Name: \TY:CL_J_1BNFE_XML_DOWNLOAD\ME:SAVE_XML_TO_SCREEN\SE:BEGIN\EI
ENHANCEMENT 0 Z_XML_MDFE_DOWNLOAD_OBJ.
*

   if IV_DOCTYPE is INITIAL.

     select SINGLE * into @data(wa_j_1bnfe_active)
       from j_1bnfe_active
      where docnum eq @IV_DOCNUM.

     if sy-subrc is INITIAL.
       CASE wa_j_1bnfe_active-MODEL.
       	WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_NFE.
          IV_DOCTYPE = 'NFE'.
       	WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_CTE.
          IV_DOCTYPE = 'CTE'.
       	WHEN ZIF_DOC_ELETRONICO=>AT_ST_MODEL_MDFE.
          IV_DOCTYPE = 'MFE'.
       ENDCASE.
     endif.

   endif.

ENDENHANCEMENT.
