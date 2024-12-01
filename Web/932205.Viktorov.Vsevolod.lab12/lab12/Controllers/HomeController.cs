using lab12.Models;
using lab12.Services;
using Microsoft.AspNetCore.Mvc;
using System.Diagnostics;
using System.Reflection.Metadata.Ecma335;

namespace lab12.Controllers
{
    public class HomeController : Controller
    {
        private readonly ILogger<HomeController> _logger;

        private CalculationService _calculationService;

        public HomeController(ILogger<HomeController> logger, CalculationService calculationService)
        {
            _logger = logger;
            _calculationService = calculationService;
        }

        private string parseData()
        {
            try
            {
                int firstNumber = int.Parse(Request.Form["firstNumber"]);
                int secondNumber = int.Parse(Request.Form["secondNumber"]);
                string opertaion = Request.Form["operation"];
                return _calculationService.calc(firstNumber, secondNumber, opertaion);
            }
            catch(Exception ex)
            {
                return ex.Message;
            }
        }

        public IActionResult Index()
        {
            return View();
        }
      
        public IActionResult Manual()
        {
            if(Request.Method == "GET")
            return View("inputForm");
            else
            {
                ViewData["result"] = parseData();
                return View("result");
            }
        }


        [HttpGet, ActionName("ManualSeparate")]
        public IActionResult ManualSeperateGet()
        {
            return View("inputForm");
        }

        [HttpPost, ActionName("ManualSeparate")]
        public IActionResult ManualSeperatePost()
        {
            ViewData["result"] = parseData();
            return View("result");
        }

        [HttpGet]
        public IActionResult ModelBindingSeperate()
        {
            return View("inputForm");
        }

        [HttpPost]
        public IActionResult ModelBindingSeperate(int firstNumber, int secondNumber, string operation)
        {
            ViewData["result"] = _calculationService.calc(firstNumber, secondNumber, operation);
            return View("result");
        }


        [HttpGet]
        public IActionResult ModelBinding()
        {
            return View("inputForm");
        }

        [HttpPost]
        public IActionResult ModelBinding(CalculationExpressionModel expressionModel)
        {
            ViewData["result"] = _calculationService.calc(expressionModel);
            return View("result");
        }

        public IActionResult Privacy()
        {
            return View();
        }

        [ResponseCache(Duration = 0, Location = ResponseCacheLocation.None, NoStore = true)]
        public IActionResult Error()
        {
            return View(new ErrorViewModel { RequestId = Activity.Current?.Id ?? HttpContext.TraceIdentifier });
        }
    }
}