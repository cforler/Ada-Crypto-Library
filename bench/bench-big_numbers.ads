-- Author: schmolli

with Crypto.Types.Big_Numbers;

pragma Elaborate_All(Crypto.Types.Big_Numbers);
pragma Optimize(Time);

generic
   Size : Positive;
   CSV  : Boolean;

package Bench.Big_Numbers is

      package Big is new Crypto.Types.Big_Numbers(Size);

    use Big;
    use Big.Utils;
    use Big.Mod_Utils;
    use Big.Binfield_Utils;

    function Bench_Big_Numbers return Boolean;

end Bench.Big_Numbers;
