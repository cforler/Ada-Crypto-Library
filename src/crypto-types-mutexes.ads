

package Crypto.Types.Mutexes is
	protected type Mutex_Type is
		entry Seize;
      procedure Release;
	private
      Owned : Boolean := False;
	end Mutex_Type;

end Crypto.Types.Mutexes;
