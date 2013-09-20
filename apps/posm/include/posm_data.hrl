-record(posm_kv,  {key, 
				   value
				  }).

%% tables bank and account
-record(posm_bank_account, {
		cubcta  :: bitstring(),
		nro_cta	:: bitstring(),
		nro_cbu :: bitstring()
	}).

-record(posm_bank, {
		cubco    :: bitstring(),	 			% Bank Id
		nombco   :: bitstring(),	 			% Bank description
		accounts :: list()  % Bank account [{cubcta, nro_cta, nro_cbu}]
	}).