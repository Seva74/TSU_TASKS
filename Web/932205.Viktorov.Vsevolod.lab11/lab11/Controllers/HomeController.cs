using lab11.Models;
using Microsoft.AspNetCore.Mvc;
using System.Diagnostics;

namespace lab11.Controllers
{
    public class HomeController : Controller
    {
        private readonly ILogger<HomeController> _logger;



        public HomeController(ILogger<HomeController> logger)
        {
            _logger = logger;
        }

        public IActionResult Index()
        {
            return View();
        }

        public IActionResult ModelCalc()
        {
            CalculationModel model = new CalculationModel();   
            return View(model);
        }

        public IActionResult ViewDataCalc()
        {
            CalculationModel model = new CalculationModel();
            ViewData["firstNumber"] = model.firstNumber;
            ViewData["secondNumber"] = model.secondNumber;
            return View();
        }

        public IActionResult ViewBagCalc()
        {
            CalculationModel model = new CalculationModel();
            ViewBag.firstNumber = model.firstNumber;
            ViewBag.secondNumber = model.secondNumber;
            return View();
        }

        public IActionResult InjectionCalc()
        {
            return View();
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