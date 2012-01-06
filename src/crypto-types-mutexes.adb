
package body Crypto.Types.Mutexes is

	protected body Mutex_Type is
		entry Seize when not Owned is
		begin
			Owned := True;
		end Seize;

		procedure Release is
		begin
			Owned := False;
		end Release;
	end Mutex_Type;

end Crypto.Types.Mutexes;



