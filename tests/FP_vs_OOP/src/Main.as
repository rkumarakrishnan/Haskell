package 
{
	import flash.display.Sprite;
	public class Main extends Sprite
	{
		
		public function Main ()
		{	
			var coins:Array = [20, 10, 5, 1]
			trace("makeChange(202, coins) = " + makeChange(202, coins));
		}
		
		public function makeChange (amount:int, coins:Array) : Array
		{
			var aux:Array = scanl(mod, amount, coins);
			var ans:Array = zipWith(div, aux, coins);
			return ans;
		}
		
		private function zipWith (op:Function, list1:Array, list2:Array) : Array
		{
			var ans:Array = new Array();
			
			for (var i:int = 0; i < Math.min(list1.length, list2.length); i++) ans.push(op(list1[i], list2[i]))
			
			return ans;
		}
		
		private function scanl (op:Function, init:int, list:Array) : Array
		{
			var ans:Array = new Array();
			
			ans.push(init);
			for (var i:int = 0; i < list.length; i++) ans[i+1] = op(ans[i], list[i])
			
			return ans;
		}
		
		private function div (a:int, b:int) : int
		{
			return a / b;
		}
		
		private function mod (a:int, b:int) : int
		{
			return a % b;
		}
	}
}